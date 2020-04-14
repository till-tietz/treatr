#' @title continuous treatment indicator function
#'
#' @description Given a binary treatment indicator for some set of geographic units, this function allows you to
#' aggregate the treatment data to a set of larger scale geographic units and create a continuous treatment
#' indicator based on the fraction of the larger geographic units' area that was treated.
#' Considering potential for mismatches in geometries between geographic units the function allows you
#' to filter out "invalid" overlaps between geographic units arising as an artefact of these mismatches,
#' by way of a user set filter value or a generic inbuilt method.
#'
#' @param input.level An sf data frame with geometries and associated binary treatment indicator. The treatment indicator should be named "treatment" and is assumed to be a numeric vector with 0 representing untreated and 1 representing treated units.
#' @param output.level An sf data frame with geometries of the geographic units the function should output a continuous treatment indicator for.
#' @param coordinate.system Four digit epsg code of the projection you wish to use. Defaults to 4326.
#' @param filter Smallest area an intersection between the input.level and output.level geographic units may have to be considered valid. All intersections with a smaller area will be filtered out. Defaults to NULL.
#' @param filter.automatic If set to TRUE the function discards all intersections between input.level and output.level geographic units that have a smaller area than the smallest input.level geographic unit. Defaults to FALSE.
#' @return A data frame of output.level geographic units and the percentage to which they were treated.
#' @export

continuous.treatment <- function(input.level, output.level, coordinate.system = 4326, filter = NULL, filter.automatic = FALSE){
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

  cont_treat <- function(x){
    area_output <- output_level %>% dplyr::slice(.,x)
    area_input <- input_level %>% dplyr::slice(., intersects[[x]])
    intersection <- sf::st_intersection(area_output, area_input)

    intersection <- intersection%>%
      dplyr::mutate(area_intersect = sf::st_area(intersection))%>%
      dplyr::mutate(area_intersect = as.character(area_intersect))%>%
      dplyr::mutate(area_intersect = as.numeric(substr(area_intersect,1,nchar(area_intersect)-5)))%>%
      dplyr::mutate(area_fraction = (area_intersect / area))%>%
      dplyr::select(-c(area))%>%
      sf::st_set_geometry(.,NULL)

    if(is.null(filter) & filter.automatic == FALSE){
      intersection <- intersection %>%
        dplyr::mutate(area_fraction = area_fraction * 100)%>%
        dplyr::group_by(treatment)%>%
        dplyr::mutate(percent_treated = sum(area_fraction))%>%
        dplyr::ungroup()
      intersection <- if(1 %in% intersection$treatment){
        intersection %>%
          dplyr::filter(treatment == 1) %>%
          dplyr::slice(.,1)
      } else {
        intersection %>%
          dplyr::filter(treatment == 0) %>%
          dplyr::slice(.,1) %>%
          dplyr::mutate(percent_treated = replace(percent_treated, 1, 0))
      }
      intersection <- intersection %>%
        dplyr::select(-c(area_intersect,area_fraction,treatment))
      return(intersection)
      ##########
    } else {
      if(!is.null(filter) & filter.automatic == FALSE){
        intersection <- intersection %>%
          dplyr::filter(area_intersect > filter) %>%
          dplyr::mutate(area_fraction = (area_fraction + ((1-(sum(area_fraction)))/nrow(.)))*100) %>%
          dplyr::group_by(treatment)%>%
          dplyr::mutate(percent_treated = sum(area_fraction)) %>%
          dplyr::ungroup()
        intersection <- if(1 %in% intersection$treatment){
          intersection %>%
            dplyr::filter(treatment == 1) %>%
            dplyr::slice(.,1)
        } else {
          intersection %>%
            dplyr::filter(treatment == 0) %>%
            dplyr::slice(.,1) %>%
            dplyr::mutate(percent_treated = replace(percent_treated, 1, 0))
        }
        intersection <- intersection %>%
          dplyr::select(-c(area_intersect,area_fraction,treatment))
        return(intersection)
        ##########
      } else {
        if(is.null(filter) & filter.automatic == TRUE){
          filter_value <- sf::st_area(input_level) %>% as.character() %>% as.numeric(substr(.,1,nchar(.)-5))
          intersection <- intersection %>%
            dplyr::filter(area_intersect > min(filter_value)) %>%
            dplyr::mutate(area_fraction = (area_fraction + ((1-(sum(area_fraction)))/nrow(.)))*100) %>%
            dplyr::group_by(treatment)%>%
            dplyr::mutate(percent_treated = sum(area_fraction)) %>%
            dplyr::ungroup()
          intersection <- if(1 %in% intersection$treatment){
            intersection %>%
              dplyr::filter(treatment == 1) %>%
              dplyr::slice(.,1)
          } else {
            intersection %>%
              dplyr::filter(treatment == 0) %>%
              dplyr::slice(.,1) %>%
              dplyr::mutate(percent_treated = replace(percent_treated, 1, 0))
          }

          intersection <- intersection %>%
            dplyr::select(-c(area_intersect,area_fraction,treatment))
          return(intersection)
        }
      }
    }
  }
  cont_treat_out <- purrr::map((1:nrow(output_level)), ~cont_treat(.x), .progress = TRUE)

  treatment_continuous <- dplyr::bind_rows(cont_treat_out, .id = "column_label")%>%
    dplyr::select(-c(column_label))
  return(treatment_continuous)
}
