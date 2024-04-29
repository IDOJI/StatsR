sub___p.vals.signif.stars = function(p.vals, show.NS = TRUE){
  p.vals = p.vals %>% as.numeric

  if(show.NS){

    ifelse(p.vals < 0.001, "***",
           ifelse(p.vals <= 0.01, "**",
                  ifelse(p.vals <= 0.05, "*",
                         ifelse(p.vals > 0.1, "HNS", "NS")))) # HNS = Highly Not Significant

  }else{

    ifelse(p.vals < 0.001, "***",
           ifelse(p.vals <= 0.01, "**",
                  ifelse(p.vals <= 0.05, "*", ""))) # NS, HNS = ""

  }

}
