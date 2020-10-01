#' Random, condition based samplig of a set of individuals
#'
#' Takes a dataframe of individuals (rows) with certain attributes (columns), and samples a given number of individuals from all possible combinations of a given set of paramters
#' For example, can sample one individual of ech combination of sex and age
#' Also allows for some control, like, "no individuals sharing the same tank and family"
#'
#' @param dataframe The dataframe to get the individuals from. Each row must be a single individual. Columns are attributes.
#' @param ofEach The attributes we want to randomise over, as a vector of column names, e.g: ofEach=c("sex","age","tank")
#' @param N_ofEach How many individuals within each combination of the "ofEach" parameters that should be selected
#' @param noShareWithin One or more sets of paremeters, where unique combinations can't be shared. For example, if we want no more than a single individual from any given family, within a tank, a set would be c("family","tank"), meaning that no individuals can have the same of both family and tank. Supplied within a list, so: list(c("family","tank")). Add more sets within the list if needed, for example: list(c("tank","father"),c("tank","mother)))
#' @param priBy If some individuals are to be prioritized over others, specify the name of the column containing prioritization info. This must ba number, and lower numbers are prioritized. (E.g, individuals with "1" are prioritized over individuals iwth "2")
#' @param useDuplis If all individuals that are selected within each combination may be be used, or just one of them.
#' @export
ransampler = function(dataframe, ofEach,nOfEach=1,noShareWithin=c(),priBy,useDuplis=F){
  message("Starting random sampling...")
  dataframe$ID_num       <- 1:nrow(dataframe)

  # just adding columns that will be in the resulting dataframe, to keep them equal
    dataframe$ID_type      <- NA
  dataframe$level        <- NA
  if(is.vector(ofEach)){
    table_combinations  <- combinationTable(dataframe, ofEach)
  }
  else {
    table_combinations <- ofEach
  }


  table_combinations %<>% combinations_getOptions(table_main=dataframe) %>%
    arrange(by=options)

  message(glue("Combinations with no options:{table_combinations %>% filter(options==0) %>% nrow()}"))
  if(table_combinations %>% filter(options==0) %>% nrow() != 0) print(table_combinations %>% filter(options==0))
   table_combinations %<>%  select(-c(options))


  if(missing(priBy)){
    dataframe$pri = 1
    priBy="pri"
  }

  table_foundIndividuals <- dataframe[0,]
  pri_max                <- dataframe[[priBy]] %>% max(na.rm=T)

  count_notFound = 0
  count_internalConflicts=0
  count_externalConflicts=0
  notFoundList="Not found:"




  ## build the found indiv table layerswise. Find first one individual of each combinatoin,
  for (count_layer in 1:nOfEach)
  {
    for (row in 1:nrow(table_combinations)){
    {
      found                       <- F
      count_level                 <- 1
      pri_cur                     <- 1
      combination                 <- table_combinations[row,] %>% as.list()
      table_thisCombinationsIndiv <- dataframe %>% listSelect(combination)


      while (found==F)
      {
        table_thisCombinationsIndiv_narrow = table_thisCombinationsIndiv

        # Narrowing: (If this is on round #1, and usediplus is F, and layers are not exhausted, take only individuals with the same values as the first one chosen for this combination
        if (count_layer != 1 & useDuplis == F & count_level != count_layer){
          previous_individual = table_foundIndividuals %>% listSelect(combination) %>% filter(level==count_level-1)

          # narrow the selection
          for (noShareCombination in noShareWithin){
            parameters = previous_individual %>% select(unlist(noShareCombination)) %>% as.list()
            table_thisCombinationsIndiv_narrow = table_thisCombinationsIndiv_narrow %>% listSelect(parameters)
          }

          # if this selection is empty, try the individual from the next layer (retry)
          if (nrow(table_thisCombinationsIndiv_narrow) == 0){
            count_level = count_level+1
            next()
          }
        }

        # Prioritizing
        table_thisCombinationsIndiv_narrow = table_thisCombinationsIndiv_narrow %>% filter(pri == pri_cur)
        if (nrow(table_thisCombinationsIndiv_narrow) == 0)
        {

          if (pri_cur == pri_max){

          found=T
          count_notFound=count_notFound+1

          table_foundIndividuals %<>% add_row(ID_type = glue("Type {row}"))
          next()
          }

          pri_cur = pri_cur+1
          next()
        }

         # PICK AN INDIVIDUAL FROM THE (POTENTIALLY NARROWED) SELECTION OF INDIVIDUALS
         ranRow = round(runif(1,1,nrow(table_thisCombinationsIndiv_narrow)))
         individual      = table_thisCombinationsIndiv_narrow[ranRow,]
         table_thisCombinationsIndiv = table_thisCombinationsIndiv %>% filter(ID_num != individual$ID_num)




         #external conflicts
         external_conflict=F
         # go through each noshare combination
         for (noShareCombination in noShareWithin){
            parameters = individual %>% select(unlist(noShareCombination)) %>% as.list()
            # remove individuals of the same type from the table
            table_notThese = table_foundIndividuals %>% listSelect(combination)
            table_conflictIndividuals = table_foundIndividuals %>% listSelect(parameters) %>% filter(!ID_num %in% table_notThese$ID_num)
            if (nrow(table_conflictIndividuals)!=0){
              external_conflict=T
              count_externalConflicts=count_externalConflicts+1
            }
         }
         if (external_conflict){
         next()
         }



         #internal conflict
         internal_conflict=F
         for (noShareCombination in noShareWithin){
            parameters = individual %>% select(unlist(noShareCombination)) %>% as.list()

            table_conflictIndividuals = table_foundIndividuals %>% listSelect(parameters) %>% listSelect(combination)
            if (nrow(table_conflictIndividuals) !=0){
              internal_conflict=T
              count_internalConflicts=count_internalConflicts+1
            }
         }
         if (useDuplis & internal_conflict){
         next()
         }

         # no conflicts!
         # store this individual and move on to the next:
         found=T
         individual$level = count_level
         individual$ID_type = glue("Type {row}")
         dataframe = dataframe %>% filter(ID_num != individual$ID_num)
         table_foundIndividuals = rbind(table_foundIndividuals, individual)

      }
    }

    }
        notFoundList = glue("{notFoundList} Layer{count_layer}={count_notFound},")
    count_notFound=0
  }
  message(notFoundList)

  message(glue("Internal conflicts: {count_internalConflicts}"))
  message(glue("External conflicts: {count_externalConflicts}"))
  message("Done")
  return(table_foundIndividuals)
}


combinationTable = function(dataframe, columns){
  dataframe %>% select(columns) %>% na.omit() %>% as.list() %>% do.call(crossing,.)
}


listSelect = function(df,l,eq=T){
  for (s in 1:length(l))
  {
    col = names(l)[s]
    val = l[s]
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
    thisIndividuals = table_main %>% listSelect(thisCombination)
    result          = thisIndividuals %>% nrow()
  })
  table_combinations$options = options
  table_combinations
}
