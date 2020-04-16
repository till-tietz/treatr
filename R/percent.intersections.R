#' @title percent overlap function 
#'
#' @description This function allows you to compute the percentage area of a larger scale geographic 
#' unit that is covered by a smaller scale geographic unit.
#' Considering potential for mismatches in geometries between geographic units the function allows you
#' to filter out "invalid" overlaps between geographic units arising as an artefact of these mismatches,
#' by way of a user set filter value or a generic inbuilt method.
#'
#' @param input.level An sf data frame.
#' @param output.level An sf data frame.
#' @param coordinate.system Four digit epsg code of the projection you wish to use. Defaults to 4326.
#' @param filter Smallest area an intersection between the input.level and output.level geographic units may have to be considered valid. All intersections with a smaller area will be filtered out. Defaults to NULL.
#' @param filter.automatic If set to TRUE the function discards all intersections between input.level and output.level geographic units that have a smaller area than the smallest input.level geographic unit. Defaults to FALSE.
#' @return A data frame named "intersections" with a column "area_percent" indicating the percentage area of an output.level geographic unit covered by a specific input.level geographic unit.
#' @export

percent.intersections <- function(input.level, output.level, coordinate.system = 4326, filter = NULL, filter.automatic = FALSE){
  if(!is.null(filter) & filter.automatic == TRUE){
    stop("both user set and automatic filters are specified. please choose one filtering method.")
  }
  if(missing(input.level)){ stop("missing input.level")
  } else {
    if(class(input.level)[[1]] != "sf"){ stop("input.level not of class sf. please input sf object with st_read function")
    } else {
      input_level <- input.level %>% sf::st_transform(coordinate.system) %>% sf::st_buffer(.,0)
    }
  }
  if(missing(output.level)){ stop("missing output.level")
  } else {
    if(class(output.level)[[1]] != "sf"){ stop("output.level not of class sf. please input sf object with st_read function")
    } else {
      output_level <- output.level %>% sf::st_transform(coordinate.system) %>% sf::st_buffer(.,0)
      output_level <- output_level %>%
        dplyr::mutate(area = sf::st_area(output_level))%>%
        dplyr::mutate(area = as.character(area))%>%
        dplyr::mutate(area = as.numeric(substr(area,1,nchar(area)-5)))
    }
  }
  intersects <- sf::st_intersects(output_level, input_level)
  
  intersect_area <- function(x){
    area_output <- output_level %>% dplyr::slice(.,x)
    area_input <- input_level %>% dplyr::slice(., intersects[[x]])
    intersection <- sf::st_intersection(area_output, area_input)
    
    intersection <- intersection%>%
      dplyr::mutate(area_intersect = sf::st_area(intersection))%>%
      dplyr::mutate(area_intersect = as.character(area_intersect))%>%
      dplyr::mutate(area_intersect = as.numeric(substr(area_intersect,1,nchar(area_intersect)-5)))%>%
      dplyr::mutate(area_fraction = ((area_intersect / area)*100))%>%
      dplyr::select(-c(area))%>%
      sf::st_set_geometry(.,NULL)
    if(is.null(filter) & filter.automatic == FALSE){
      return(intersection)
    } else {
      if(!is.null(filter) & filter.automatic == FALSE){
        intersection <- intersection %>%
          dplyr::filter(area_intersect > filter) %>%
          dplyr::mutate(area_fraction = area_fraction + ((100-(sum(area_fraction)))/nrow(.)))
        return(intersection)
      } else {
        if(is.null(filter) & filter.automatic == TRUE){
          filter_value <- sf::st_area(input_level) %>% as.character() %>% as.numeric(substr(.,1,nchar(.)-5))
          intersection <- intersection %>%
            dplyr::filter(area_intersect > min(filter_value)) %>%
            dplyr::mutate(area_fraction = area_fraction + ((100-(sum(area_fraction)))/nrow(.)))
          return(intersection)
          }
        }
    }
  }
  intersect_area_out <- purrr::map((1:nrow(output_level)), ~intersect_area(.x))
  intersections <- dplyr::bind_rows(intersect_area_out, .id = "column_label")%>%
    dplyr::rename(area_percent = area_fraction)
    dplyr::select(-c(column_label))
  return(intersections)
}
