#' Random, condition based samplig of a set of individuals
#'
#' Takes a dataframe of individuals (rows) with certain attributes (columns), and samples a given number of individuals from all possible combinations of a given set of paramters
#' For example, can sample one individual of ech combination of sex and age
#' Also allows for some control, like, "no individuals sharing the same treatment and family"
#'
#' @param dataframe The dataframe to get the individuals from. Each row must be a single individual. Columns are attributes.
#' @param ofEach The attributes we want to randomise over, as a vector of column names, e.g: ofEach=c("sex","age","tank")
#' @param N_ofEach How many individuals within each combination of the "ofEach" parameters that should be selected
#' @param noShareWithin One or more sets of paremeters, where unique combinations can't be shared. For example, if we want no more than a single individual from any given family, within a tank, a set would be c("family","tank"), meaning that no individuals can have the same of both family and tank. Supplied within a list, so: list(c("family","tank")). Add more sets within the list if needed, for example: list(c("tank","father"),c("tank","mother)))
#' @param priBy If some individuals are to be prioritized over others, specify the name of the column containing prioritization info. This must ba number, and lower numbers are prioritized. (E.g, individuals with "1" are prioritized over individuals iwth "2")
#' @param useDuplis If all individuals that are selected within each combination may be be used, or just one of them.
#' @export
ransampler = function(table,ofEach,except,nOfEach=1,noShareWithin=c(),priBy,useDuplis=F,identifier="",returnCombTable=F,runs=1,reshuffle_combinationTable=F){
  require(glue)
  require(tidyverse)
  require(magrittr)

  # Argument handling ---------------------------------------

  # ofEach can be:
  #   A vector: Is translated to a combination table
  #   A data.frame: is assumed to be a combination table
  #   A list of data frames: Is merged into a single data.frame
  #   Empty,
  if(is.vector(ofEach)){
    table_combinations  <- combinationTable(table, ofEach)
  }
  else if (is.list(ofEach)){
    table_combinations <- bind_rows(ofEach)
  }

  if (!missing(nOfEach))
  {
    table_combinations$nOfThis = nOfEach
  }

  # if exceptionsa are used:
  if (!missing(except)){
    for (exception in except){
      table_combinations <- table_combinations %>% anti_join(listFilter(df=table_combinations,l=exception,eq=T),by=ofEach)
    }
  }

  if(missing(priBy)){
    table$internalPri = 1
    priBy="internalPri"
  }

  if (returnCombTable==T){
    return(table_combinations %>%
    combinations_getOptions(table_main=table) %>%
    #arrange(n_options) %>%
    as_tibble())
  }
  else print(
    table_combinations %>%
    combinations_getOptions(table_main=table) %>%
    #arrange(n_options) %>%
    as_tibble(),
  n=500)

  table_combinations_ori <- table_combinations
  table_selected_best <- NULL


  for (run_current in 1:runs)
  {

    if(runs > 1) message(glue("Run {run_current} of {runs}"))

    if (run_current !=1 & reshuffle_combinationTable==T) table_combinations <- shuffle_rows(table_combinations_ori)
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
    while (count_layer <= max(table_combinations$nOfThis))
    {
      for (row in 1:nrow(table_combinations))
      {
        # This section tries to find an individual of a given combination
        # Following is just setup
        is_found             <- F
        count_level          <- 1
        count_pri            <- 1
        ID_type_cur          <- as.character(glue("Type {identifier}-{row}"))
        curCombination       <- table_combinations[row,] %>% as.list() %>% remove_listItem("nOfThis")
        table_curCombination <- table_main %>% listFilter(curCombination)

        # check if we've reached the limit of how many layers this curCombination wants
        if (count_layer > table_combinations[row,]$nOfThis)
        {
          next()
        }

        # This is the loop that keeps looking for an indvidivual to fill that curCombination
        while (is_found==F)
        {
          table_curCombination_narrow = table_curCombination

          # Prioritizing
          table_curCombination_narrow <- table_curCombination_narrow %>% filter(!!rlang::sym(priBy) == count_pri)

          # Checking if this priority is empty
          if (nrow(table_curCombination_narrow) == 0)
          {
            if (count_pri == max(table_main[[priBy]],na.rm=T))
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

          # Optimizing when useduplis==F
          # The following tries its best to have individuals of the same combination use the same noshare combinations
          # At some point, this is not going to work and this step is skipped.
          if (useDuplis == F & count_layer != 1 & count_level != count_layer)
          {
            previous_individual <<- table_selected %>% filter(ID_type == ID_type_cur) %>% filter(level==count_level)

            if (previous_individual %>% nrow() == 0)
            {
              count_level <- count_level+1
              next()
            }

            for (noShareCombination in noShareWithin)
            {
              parameters_prevIndiv <- previous_individual %>% select(noShareCombination)
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
           for (noShareCombination in noShareWithin)
           {
              table_notThese <- table_selected %>% listFilter(curCombination)
              cur_noShareParameters <- curIndividual %>% select(unlist(noShareCombination)) %>% as.list()
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
           if (useDuplis & internal_conflict)
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


  if (run_current != 1 & useDuplis==T) {
    if(sum(is.na(table_selected$level)) < sum(is.na(table_selected_best$level))) table_selected_best <- table_selected
  }
  else if(run_current != 1 & useDuplis==F){
    table_selected_layer1 <- table_selected[which(table_selected$layer == 1),]
    table_selected_best_layer1 <- table_selected_best[which(table_selected_best$layer == 1),]
    if(sum(is.na(table_selected_layer1$level)) < sum(is.na(table_selected_best_layer1$level))) table_selected_best <- table_selected
  }
  else {
    table_selected_best <- table_selected
    }

  }
  return(table_selected_best)
}

#' @export
combinationTable = function(dataframe, columns){
  dataframe %>% select(columns) %>% na.omit() %>% as.list() %>% do.call(crossing,.)
}

# SKIPS NA values
listFilter = function(df,l,eq=T){

  for (s in 1:length(l))
  {
    col = names(l)[s]
    val = l[s]


    if (is.na(val))
    {
      next()
    }
    if(col %in% colnames(df)){
      if(eq)  df = df %>% filter(!!sym(col)==val)
      else    df = df %>% filter(!!sym(col)!=val)
    }
  }
  return(df)
}


combinations_getOptions = function(table_combinations, table_main){
  options = table_combinations %>% apply(MARGIN=1,FUN=function(x){

    thisCombination = x %>% as.list()
    pip <<- thisCombination
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
