#' Random, condition based samplig of a set of individuals
#'
#' Takes a dataframe of individuals (rows) with certain attributes (columns), and samples a given number of individuals from all possible combinations of a given set of paramters
#' For example, can sample one individual of ech combination of sex and age
#' Also allows for some control, like, "no individuals sharing the same treatment and family"
#' Some lingo:
#' \itemize{
#' \item type: a specific combination of attributes, e.g "color:red, size:large", as define  
#' \item combination table: A table that tells the sampler what you are looking for. In the table, each attribute has one column, and each row gives a different combination of attributes. The table is generated automatically based on your dataset and the "ofeach" parameter, but you can also supply it manualy.
#' }
#'
#' @param dataframe The dataframe to get the individuals from. Each row must be a single individual. Columns are attributes.
#' @param ofeach The attributes we want to randomise over, as a vector of column names, e.g: ofeach=c("sex","age","tank"). Can also be a combination table (data frame).
#' @param n_ofeach How many individuals within each combination (type) of the "ofeach" parameters that should be selected
#' @param no_share One or more sets of paremeters, where unique combinations can't be shared. For example, if we want no more than a single individual from any given family, within a tank, a set would be c("family","tank"), meaning that no individuals can have the same of both family and tank. Supplied within a list, so: list(c("family","tank")). Add more sets within the list if needed, for example: list(c("tank","father"),c("tank","mother)))
#' @param pri_by If some individuals are to be prioritized over others, specify the name of the column containing prioritization info. This must ba number, and lower numbers are prioritized. (E.g, individuals with "1" are prioritized over individuals iwth "2")
#' @param use_dupli If no_share should be enforced for individuals of the same combiantion (type). In other words, if both individuals of same type are planned to be used, or if one is just backup. 
#' @param identifier Optional. A name that will be added to the "ID_type" column. 
#' @param return_combtable if T: Returns the combination table instead of searching for individuals.
#' @param runs If T: Runs the search multiple times, and returns the run with the highest number of successfully selected individuals
#' @param reshuffle_combtable If T: When using multiple runs, will reshuffle the order of the combination table, can help with finding individuals of problematic types (combinations)
#' @export
ransampler = function(table,ofeach,except,n_ofeach=1,no_share=c(),pri_by,use_dupli=F,identifier="",return_combtable=F,runs=1,reshuffle_combtable=F){
  require(glue)
  require(tidyverse)
  require(magrittr)

  # Argument handling ---------------------------------------

  # ofeach can be:
  #   A vector: Is translated to a combination table
  #   A data.frame: is assumed to be a combination table
  #   A list of data frames: Is merged into a single data.frame
  #   Empty,
  if(is.vector(ofeach)){
    table_combinations  <- combinationTable(table, ofeach)
  }
  else if (is.list(ofeach)){
    table_combinations <- bind_rows(ofeach)
  }

  if (!missing(n_ofeach))
  {
    table_combinations$n_ofeach = n_ofeach
  }

  # if exceptionsa are used:
  if (!missing(except)){
    for (exception in except){
      table_combinations <- table_combinations %>% anti_join(listFilter(df=table_combinations,l=exception,eq=T),by=ofeach)
    }
  }

  if(missing(pri_by)){
    table$internalPri = 1
    pri_by="internalPri"
  }

  if (return_combtable==T){
    return(table_combinations %>%
    combinations_getOptions(table_main=table) %>%
    #arrange(n_options) %>%
    as_tibble())
  }
  else {
    message("Searching using the following table of combinations:")
    print(
    table_combinations %>%
    combinations_getOptions(table_main=table) %>%
    #arrange(n_options) %>%
    as_tibble(),
    n=500)
  }
  
  table_combinations_ori <- table_combinations
  table_selected_best <- NULL


  for (run_current in 1:runs)
  {

    if(runs > 1) message(glue("Run {run_current} of {runs}"))

    if (run_current !=1 & reshuffle_combtable==T) table_combinations <- shuffle_rows(table_combinations_ori)
    else table_combinations <- table_combinations_ori


    # Variables setup ----------------------------------------
    table_main <- table
    table_main$ID_num  <- 1:nrow(table_main)
    table_main$ID_type <- ""
    table_main$level   <- NA
    table_main$layer   <- NA
    table_selected <- table_main[0,]

    count_layer <- 1
    ## build the found indiv table layerswise. Find first one individual of each combinatoin,
    while (count_layer <= max(table_combinations$n_ofeach))
    {
      for (row in 1:nrow(table_combinations))
      {
        # This section tries to find an individual of a given combination
        # Following is just setup
        is_found             <- F
        count_level          <- 1
        count_pri            <- 1
        ID_type_cur          <- as.character(glue("Type {identifier}-{row}"))
        curCombination       <- table_combinations[row,] %>% as.list() %>% remove_listItem("n_ofeach")
        table_curCombination <- table_main %>% listFilter(curCombination)

        # check if we've reached the limit of how many layers this curCombination wants
        if (count_layer > table_combinations[row,]$n_ofeach)
        {
          next()
        }

        # This is the loop that keeps looking for an indvidivual to fill that curCombination
        while (is_found==F)
        {
          table_curCombination_narrow = table_curCombination

          # Prioritizing
          table_curCombination_narrow <- table_curCombination_narrow %>% filter(!!rlang::sym(pri_by) == count_pri)

          # Checking if this priority is empty
          if (nrow(table_curCombination_narrow) == 0)
          {
            if (count_pri == max(table_main[[pri_by]],na.rm=T))
            {
              is_found <- T
              curCombination["ID_type"] <-  ID_type_cur
              curCombination["layer"]   <-  count_layer
              table_selected %<>% bind_rows(curCombination)
              next()
            }
            count_pri = count_pri+1
            next()
          }

          # Optimizing when use_dupli==F
          # The following tries its best to have individuals of the same combination use the same noshare combinations
          # At some point, this is not going to work and this step is skipped.
          if (use_dupli == F & count_layer != 1 & count_level != count_layer)
          {
            previous_individual <- table_selected %>% filter(ID_type == ID_type_cur) %>% filter(level==count_level)
            
            if (previous_individual %>% nrow() == 0)
            {
              count_level <- count_level+1
              next()
            }

            for (noShareCombination in no_share)
            {
              parameters_prevIndiv <- previous_individual %>% select(all_of(noShareCombination))
              table_curCombination_narrow <- table_curCombination_narrow %>% listFilter(parameters_prevIndiv)
            }
            if (nrow(table_curCombination_narrow) == 0)
            {
              count_level <- count_level+1
              next()
            }
          }

           # Pick an individual
           ranRow <- round(runif(1,1,nrow(table_curCombination_narrow)))
           curIndividual <- table_curCombination_narrow[ranRow,]
           table_curCombination <- filter(table_curCombination, ID_num != curIndividual$ID_num)
          

           external_conflict <- F
           internal_conflict <- F
           # go through each noshare combination
           for (noShareCombination in no_share)
           {
              table_notThese <- table_selected %>% listFilter(curCombination)
              cur_noShareParameters <- curIndividual %>% select(all_of(unlist(noShareCombination))) %>% as.list()
              table_conflictIndividuals_ext <- table_selected %>% listFilter(cur_noShareParameters) %>% filter(!ID_num %in% table_notThese$ID_num)
              table_conflictIndividuals_int <- table_selected %>% listFilter(cur_noShareParameters) %>% listFilter(curCombination)

              if (nrow(table_conflictIndividuals_int) !=0)
              {
                internal_conflict <- T
              }
              if (nrow(table_conflictIndividuals_ext) !=0)
              {
                external_conflict <- T
              }
           }

           if (external_conflict)
           {
            next()
           }
           if (use_dupli & internal_conflict)
           {
            next()
           }

           # no conflicts!
           # store this individual and move on to the next:
           is_found <- T
           curIndividual$level <- count_level
           curIndividual$ID_type <- ID_type_cur
           curIndividual$layer <- count_layer
           table_main <- table_main %>% filter(ID_num != curIndividual$ID_num)
           table_selected <- bind_rows(table_selected, curIndividual)
        }
      }
      count_layer <- count_layer+1
    }

  if(runs > 1) message(glue("Missing: {sum(is.na(table_selected$level))}"))


  if (run_current != 1 & use_dupli==T) {
    if(sum(is.na(table_selected$level)) < sum(is.na(table_selected_best$level))) table_selected_best <- table_selected
  }
  else if(run_current != 1 & use_dupli==F){
    table_selected_layer1 <- table_selected[which(table_selected$layer == 1),]
    table_selected_best_layer1 <- table_selected_best[which(table_selected_best$layer == 1),]
    if(sum(is.na(table_selected_layer1$level)) < sum(is.na(table_selected_best_layer1$level))) table_selected_best <- table_selected
  }
  else {
    table_selected_best <- table_selected
    }

  }
  return(table_selected_best %>% select(-level) %>% rename(n_of_type = layer))
}

#' @export
combinationTable = function(dataframe, columns){
  dataframe %>% select(all_of(columns)) %>% na.omit() %>% as.list() %>% do.call(crossing,.)
}

# SKIPS NA values
listFilter = function(df,l,eq=T){
  for (s in 1:length(l))
  {
    col = names(l)[s]
    val = l[s]
    # to make val not be a tibble (causes warning with filter())
    val = val[[1]]

    if (is.na(val))
    {
      next()
    }
    if(col %in% colnames(df)){
      if(eq)  df <- df %>% filter(!!sym(col)==val)
      else    df <- df %>% filter(!!sym(col)!=val)
    }
  }
  return(df)
}


combinations_getOptions = function(table_combinations, table_main){
  options = table_combinations %>% apply(MARGIN=1,FUN=function(x){

    thisCombination = x %>% as.list()
    pip <- thisCombination
    thisIndividuals = table_main %>% listFilter(thisCombination)
    result          = thisIndividuals %>% nrow()
    result
  })
  table_combinations$n_options = options
  table_combinations
}

remove_listItem <- function(list,item){
  list[item]=NULL
  list
  }

shuffle_rows <- function(df){
  df <- df[sample(1:nrow(df)),]
  }
