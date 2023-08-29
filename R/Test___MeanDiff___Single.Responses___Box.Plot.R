Test___MeanDiff___Single.Responses___Box.Plot = function(Data,
                                                         Results,
                                                         label.as.p.val = FALSE,
                                                         group.comparison=FALSE,
                                                         lines.connecting.medians=FALSE,
                                                         save.path=NULL){
  #=============================================================================
  # install.package
  #=============================================================================
  install_packages(c("EnvStats", "ggpubr")) %>% invisible




  #=============================================================================
  # Define Variables used
  #=============================================================================
  Group_Var = Results$Group
  Response_Var = Results$Response




  #=============================================================================
  # Finding significant comparing groups
  #=============================================================================
  Which_Signif = which(! Results$Significance %in% c("HNS", "NS"))
  Results_Signif = Results[Which_Signif, ]


  # Post.Hoc  = Results[[2]]
  # if(!is.null(Post.Hoc)){
  #   Post.Hoc_Signif = Post.Hoc[Post.Hoc$PostHoc_p.value_adj <= alpha_PostHoc, ]
  #   Post.Hoc_Signif_tibble = Post.Hoc_Signif %>% dplyr::select(group1, group2, PostHoc_p.value_adj, PostHoc_p.value.signif)
  # }






  #=============================================================================
  # pallette
  #=============================================================================
  install_packages("RColorBrewer")
  # Step 1: Generate palette
  all_colors <- brewer.pal(12, "Set3")  # 12 is the maximum for Set3

  # Step 2: Filter out undesired color
  filtered_colors <- all_colors[all_colors != "#FFFFB3"]

  # Step 3: Check if you need more colors
  n_colors <- Data[, Group_Var] %>% unlist %>% unique %>% length

  if (length(filtered_colors) < n_colors) {
    # This is just an example: you might want to add a color or generate colors in another way
    filtered_colors <- c(filtered_colors, "#FF0000")
  }

  # Use 'filtered_colors' in your plot
  colors <- filtered_colors[1:n_colors]






  #=============================================================================
  # Boxplot
  #=============================================================================
  p1 <- ggpubr::ggboxplot(data = Data,
                          x = Group_Var,
                          y = `Response_Var`,
                          color = Group_Var,
                          palette = colors,
                          # shape = Group_Var,
                          size = 0.5,
                          add = "jitter",
                          add.params = list(size=0.5))




  #=============================================================================
  # pointing out mean
  #=============================================================================
  p1 = p1 +  stat_summary(fun = mean, geom = "point", shape = 10, size = 4, color = "red")




  #=============================================================================
  # adding lines connecting neighboing medians
  #=============================================================================
  if(lines.connecting.medians){
    p1 = p1 + stat_summary(fun = median, geom = "line", group = 1, aes(group = 1), color = "blue")
  }





  #=============================================================================
  # Legend text & title size
  #=============================================================================
  p1 = p1 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 15, face = "bold"))




  #=============================================================================
  # Label angle
  #=============================================================================
  p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))






  #=============================================================================
  # Label bold
  #=============================================================================
  p2 = p1 + ggpubr::font("xlab", size = 20, face = "bold") + ggpubr::font("ylab", size = 20, face = "bold")






  #=============================================================================
  # Adding p-values on comparing groups
  #=============================================================================
  # 두 그룹 사이의 비교 옵션 넣기
  if(group.comparison){
    if(label.as.p.val){
      labeling = "p.vals.adj"
      label.size = 2
    }else{
      labeling = "Significance"
      label.size = 5
    }

    p3 = p2 + ggpubr::stat_pvalue_manual(Results_Signif,
                                         y.position = 1.1*max(Data[,Response_Var] %>% unlist %>% as.numeric),
                                         step.increase = 0.1,
                                         label = labeling,
                                         label.size = 5,
                                         bracket.size = 0.8)
  }else{
    p3 = p2
  }




  # if(is.null(Post.Hoc) && nrow(Mean.Diff_Signif)>0){
  #
  #
  # }else{
  #
  #   if(label.as.p.val){
  #     labeling = "PostHoc_p.value_adj"
  #     label.size = 2
  #     Post.Hoc_Signif_tibble$PostHoc_p.value_adj = Post.Hoc_Signif_tibble$PostHoc_p.value_adj %>% format(scientific = F)
  #   }else{
  #     labeling = "PostHoc_p.value.signif"
  #     label.size = 5
  #   }
  #   p3 = p2 + ggpubr::stat_pvalue_manual(Post.Hoc_Signif_tibble,
  #                                        y.position = 1.1*max(Data[,Response_Var] %>% unlist %>% as.numeric),
  #                                        step.increase = 0.1,
  #                                        label = labeling,
  #                                        label.size = label.size,
  #                                        bracket.size = 0.8)
  #
  #
  # }
  #
  #


  ############################################################################
  # Adding sample size
  ############################################################################
  p4 = p3 + EnvStats::stat_n_text(text.box = F, size = 4)




  #=============================================================================
  # Change font size
  #=============================================================================
  p5 = p4 + theme(text = element_text(size = 10)) # change text size of theme components





  #=============================================================================
  # Change font size
  #=============================================================================
  if(!is.null(save.path)){
    ggsave(filename = paste0(save.path, "/[Boxplot] ", Group_Var, " vs ", Response_Var, ".png"), plot = p5, device = "png", dpi = 300)
  }






  #=============================================================================
  # Return
  #=============================================================================
  return(p5)
}






# ############################################################################
# # Creating Background of a plot
# ############################################################################
# p1 = ggpubr::ggboxplot(data    = Data,
#                        x       = Group_Var,
#                        y       = Response_Var,
#                        color   = Group_Var,
#                        #palette = ,
#                        add     = "jitter",
#                        shape   = Group_Var)








# p3 = p2 + ggpubr::stat_compare_means(label.y = 50) # Add global p-value


# # Violin plots with box plots inside
# p3 <- ggviolin(Data, x = "dose", y = "len", fill = "dose",
#                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#                add = "boxplot", add.params = list(fill = "white"))+
#   stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
# stat_compare_means(label.y = 50)        # Add global the p-value



#
# p = test_____MeanDiff___Single.Response___ggstats(# data & variables
#   Data,
#   Group_Var,
#   Response_Var,
#   # test Results
#   alpha_ANOVA
#   is.normal,
#   is.Equal.Var,
#   # plotting options
#   title)
# #
#   #############################################################################################
#
#     # post-hoc alpha
#     which_posthoc_sig = which(MeanDiff.Data$PostHoc_p.val <= posthoc_alpha)
#     comparisons = list()
#     if(length(which_posthoc_sig)>0){
#       for(i in 1:length(which_posthoc_sig)){
#         comparisons[[i]] = MeanDiff.Data[which_posthoc_sig[i],c("Group_1", "Group_2")]
#       }
#     }
#     if(length(comparisons)==0){
#       p = MeanDiff_Plotting(Results.subtitle, pairwise.comparisons = F)
#     }else{
#       p = MeanDiff_Plotting(Results.subtitle, pairwise.comparisons = T)
#       # if(is.normal){
#       #   # p = p + ggsignif::geom_signif(comparisons = comparisons,
#       #   #                               test = "t.test",
#       #   #                               map_signif_level = TRUE,
#       #   #                               col = 2,
#       #   #                               size = 1,
#       #   #                               annotations = c("***")) %>% suppressWarnings()
#       # }
#       if(M_Results.Data$MeanDiff_p.val[1] <= anova_alpha){
#         # Saving images
#         ggplot2::ggsave(filename = filename, plot = p, path = path, units="px", dpi = 300, limitsize = T)
#       }
#
#     }
#
#
#   }else{
#     MeanDiff.Data = M_Results.Data
#   }
#

# ############################################################################
# # Palette
# ############################################################################
#
# Plot___N.Palette = function(n){
#   if(n==3){
#     palette = c("#00AFBB", "#E7B800", "#FC4E07")
#   }if(n==4){
#     palette = c("#00AFBB", "#E7B800", "#FC4E07", )
#   }else{
#     palette = NULL
#   }
#   return(palette)
# }


