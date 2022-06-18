



clean_license_name<-function(license){

  # This is an evolving function. 
  # Can be extended as new licenses are added
  
  library(stringr)

  # Lowercase for easier pre-processing
  license<-tolower(license)
  
  # Make spelling of "license" consistent
  license<-gsub("licence","license",license)
  
  # Get rid of all "file license(s)" (for now)
  license<-gsub("\\s?[|+]?\\s?file license","",license) 
  
  # First split into individual licenses and clean each separately 
 
  split_license<-strsplit(license,split="\\s[|+]\\s")
  
  # Keep track of whether the license is compound (|) or singular (0)
  if(grepl("\\|",license)){split_license[[2]]<-"|"}
  else{split_license[[2]]<-0}
  
  # Some licenses will only have file license, in this case `split_license` will be a character vector of length 0
  # Ignore these cases (with if statement) for now and handle them in following else statement
  if(length(split_license[[1]])>0){
    for(i in 1:length(split_license[[1]])){ 
      
      # MIT
      if(grepl("\\bmit",split_license[[1]][i])){
        split_license[[1]][i]<-"MIT"
      }
      
      # BSD_3_clause
      else if(grepl("bsd",split_license[[1]][i])
              &grepl("3",split_license[[1]][i])){
        split_license[[1]][i]<-"BSD_3_clause"
      }
      
      # Apache 2.0
      else if(grepl("apache",split_license[[1]][i])
              &grepl("2",split_license[[1]][i])
              &!grepl(">",split_license[[1]][i])){
        split_license[[1]][i]<-"Apache 2.0"
      }
      
      # LGPL-2.1
      else if(grepl("lgpl-2.1",split_license[[1]][i])){
        split_license[[1]][i]<-"LGPL-2.1"
      }
      
      # LGPL (>=2.1)
      else if(grepl("lgpl \\(>= 2.1\\)",split_license[[1]][i])){ 
        split_license[[1]][i]<-"LGPL (>=2.1)"
      }
      
      # LGPL-3
      else if(grepl("lgpl-3",split_license[[1]][i])){
        split_license[[1]][i]<-"LGPL-3"
      }
      
      # LGPL (>=3)
      else if(grepl("lgpl \\(>= 3",split_license[[1]][i])){
        split_license[[1]][i]<-"LGPL (>=3)"
      }
      
      # GPL-2
      else if((grepl("\\bgpl",split_license[[1]][i])|grepl("\\bgnu general",split_license[[1]][i]))
              &grepl("2",split_license[[1]][i])&!grepl(">",split_license[[1]][i])
              &!grepl("<",split_license[[1]][i])){
        split_license[[1]][i]<-"GPL-2"
      }
      
      # GPL (>=2)
      else if((grepl("\\bgpl",split_license[[1]][i])|grepl("\\bgnu general",split_license[[1]][i]))
              &grepl(">",split_license[[1]][i])
              &grepl("2",split_license[[1]][i])
              &!grepl("3",split_license[[1]][i])){
        split_license[[1]][i]<-"GPL (>=2)"
      }
      
      # GPL-3
      else if((grepl("\\bgpl",split_license[[1]][i])|grepl("\\bgnu general",split_license[[1]][i]))
              &grepl("3",split_license[[1]][i])
              &!grepl(">",split_license[[1]][i])
              &!grepl("<",split_license[[1]][i])){
        split_license[[1]][i]<-"GPL-3"
      }
      
      # GPL (>=3)
      else if((grepl("\\bgpl",split_license[[1]][i])|grepl("\\bgnu general",split_license[[1]][i]))
              &grepl(">",split_license[[1]][i])
              &grepl("3",split_license[[1]][i])){ 
        split_license[[1]][i]<-"GPL (>=3)"
      }
      
      # Affero GPL-3
      else if((grepl("affero",split_license[[1]][i])|grepl("agpl",split_license[[1]][i]))
              &grepl("3",split_license[[1]][i])){
              #&!grepl(">",split_license[[1]][i]) 
        split_license[[1]][i]<-"Affero GPL-3"
      }
      
      # MPL
      else if(grepl("mpl-1.1",split_license[[1]][i])
              |grepl("mozilla public license 1.1",split_license[[1]][i])){ 
        split_license[[1]][i]<-"MPL-1.1"
      }
      
    }
  }
  
  # Handle licenses that consist only of "file license"
  else{
    split_license[[1]]<-""
  }

  return(split_license)
  
}

check_compatibility<-function(from,to){
  
  # `from` and `to` are both list vector arguments indicating the [[1]] license (`from`) whose compatibility (with `to`) is to be determined
  # and [[2]] whether the license is compound (|) or singular (0).
  # Both `from` and `to` will be cleaned licenses from the output of `clean_license_name()`.
  
  # Updated `FLOSScompatbility` matrix
  FLOSScompatibility <- matrix(
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
      0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0,
      0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0,
      0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
      0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0,
      0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
      
      
    ),
    nrow = 13, ncol = 13, byrow = TRUE,
    dimnames = list(c('MIT','BSD_3_clause','Apache 2.0','LGPL-2.1','LGPL (>=2.1)'
                      ,'LGPL-3','LGPL (>=3)','GPL-2','GPL (>=2)','GPL-3','GPL (>=3)'
                      ,'Affero GPL-3','MPL-1.1'),
                    c('MIT','BSD_3_clause','Apache 2.0','LGPL-2.1','LGPL (>=2.1)'
                      ,'LGPL-3','LGPL (>=3)','GPL-2','GPL (>=2)','GPL-3','GPL (>=3)'
                      ,'Affero GPL-3','MPL-1.1'))
                    )
  
  # If license type is not encoded, `comp_matrix` will be set to NA
  comp_matrix<-tryCatch(FLOSScompatibility[from[[1]],to[[1]]],error=function(e) NA) 
  

  # OR relationship : at least one of the licenses need to be compatible
  # If comp_matrix is length 1 (license is singlar), `comp` will be set accordingly as well
  comp<-any(as.logical(comp_matrix))
  
  return(comp)
}




compute_compatibility_inner<-function(pkg_net,pkg,first=FALSE){ 
  
  # `pkg_net` is obtained from build_network function. This will be the same for all packages
  # It is computationally inefficient to compute `pkg_net` within the function so it should be computed outside
  # then passed into the function as an argument.
  # `pkg` is a character vector argument indicating name of package of interest
  # `first` is a boolean value; TRUE to limit the computation to the first generation of dependencies;
  # FALSE to compute for all generations
  
  library(cranly)
  
  # Build dependence tree (from `cranly`) for specified package (`pkg`)
  pkg_tree<-build_dependence_tree(pkg_net, pkg)
  
  # `edges` will return a dataframe with two columns: $from and $to
  # which will give all the dependency relationships between each child package ($from) and parent package ($to)
  # for all generations of `pkg`
  edges<-pkg_tree$edges[,c("from","to")]
  
  # Some packages may have 0 dependencies or only depend on/import base packages
  # This would equate to having 0 compatibility issues
  # Which would give a compatibility metric of 1 
  if(nrow(edges)==0){
    print("Package has 0 non-base dependencies")
    return(1)
  }
  
  # if user wants to limit to first generation 
  if(first==TRUE){
    edges<-edges[edges$to==pkg, ]
  }
  
  # Add a $compatibility column to be filled in later
  edges$compatibility<-rep(NA,nrow(edges))
  
  # Create a dataframe indicating the license for each package in `edges`.
  licenses<-pkg_tree$nodes[c("package", "license","generation")]
  
  # Loop through each row (dependency relationship) in `edges` 
  for(i in 1:nrow(edges)){
    
    # Assign to `from` and `to` the name of the license
    # corresponding to the respective ($from or $to) package for ith dependency relationship
    from<-licenses[licenses$package==edges[i,"from"],"license"]
    to<-licenses[licenses$package==edges[i,"to"],"license"]
    
    # Clean `from` and `to`
    from<-clean_license_name(from)
    to<-clean_license_name(to)
    
    # Add logical value (if `from` and `to` are compatible) to $compatibility column of `edges`
    edges[i,"compatibility"]<-check_compatibility(from,to)

  }
  
  print(edges)
  
  # Compute compatibility metric for each group $j
  # Each unique value in `to` column of `edges` is a group
  comp_matrix<-data.frame("j"=1:length(unique(edges$to))
                            ,"group_j"=unique(edges$to)
                            ,"generation_j"=rep(NA,length(unique(edges$to)))
                            ,"N_j"=rep(NA,length(unique(edges$to)))
                            ,"compatibility_j"=rep(NA,length(unique(edges$to))))
  
  
  for(grp in unique(comp_matrix$group_j)){
    
    # Generation of group. All members (pairs) of group are same generation, but different groups can have same generation.
    # And recode $generation_j to be >=1
    comp_matrix[comp_matrix$group==grp,"generation_j"]<-licenses[licenses$package==grp,"generation"]*(-1)+1
    
    # Size of group
    comp_matrix[comp_matrix$group==grp,"N_j"]<-nrow(edges[edges$to==grp,])
    
    # Entire group get a compatibility score of FALSE if any single pair is incompatible
    comp_matrix[comp_matrix$group==grp,"compatibility_j"]<-all(edges[edges$to==grp,"compatibility"])
    
  }
  
  print(comp_matrix)
  
  # Throw a warning message if there are unknown (not accounted for in `FLOSScompatibility` matrix) licenses
  if(is.na(sum(edges[["compatibility"]]))){warning("Interpret with caution. Some licenses are not encoded.
                                                   Pairs with un-encoded licenses have been assumed to have compatibility issues.")}
  
  # Compute compatibility metric based off formula
  comp_metric<-sum(comp_matrix$N_j*comp_matrix$compatibility_j/comp_matrix$generation_j,na.rm=TRUE)/
    sum(comp_matrix$N_j/comp_matrix$generation_j,na.rm=TRUE)
  
  return(comp_metric)

}

compute_compatibility<-function(pkg,first=FALSE){
  
  # Wrapper function for `compute_compatibility_inner` for user friendliness
  return(compute_compatibility_inner(package_net, pkg, first)) 
}



