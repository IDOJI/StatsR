SUB___P.vals.Signif.Stars = function(p.vals){
  p.vals = p.vals %>% as.numeric
  ifelse(p.vals < 0.001, "***",
         ifelse(p.vals <= 0.01, "**",
                ifelse(p.vals <= 0.05, "*",
                       ifelse(p.vals > 0.1, "HNS", "NS")))) # HNS = Highly Not Significant
}
