library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Scatterplot application (standard data)"),
  
  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    
    h2(helpText("Intro")),
    helpText("Tällä aplikaatiolla voi kokeilla eri makromuuttujien yhteyksiä", 
             "artikkelimme selitettäviin muuttujiin.",
             "Selittävät muuttujat ovat peräisin Quality of Government Instituten",
             "STANDARD datasta",
             "Voit siis valita ainoastaan x-akselin muuttujan."),
    
    
    #     h3(helpText("Pick variable X")),
    #     textInput("varx", "x-variable code:", "perGini"),
    
    selectInput("predictor", "valitse selittävä muuttuja:", 
                choices = c("socialBlame","individualBlame","individualFate","socialFate","notStated","dontKnow","socialBlameRec","individualBlameRec","individualFateRec","socialFateRec","perGini","wbgi_vae.x","group_fine","group_general","gdpChange","al_ethnic","al_language","al_religion","ciri_assn","ciri_disap","ciri_dommov","ciri_elecsd","ciri_empinx_new","ciri_formov","ciri_injud","ciri_kill","ciri_physint","ciri_polpris","ciri_relfre_new","ciri_speech","ciri_tort","ciri_wecon","ciri_wopol","ciri_worker","dpi_auton","dpi_cemo","dpi_checks","dpi_dmmo","dpi_eipc","dpi_exelec","dpi_finter","dpi_gf","dpi_gpage1","dpi_gprlc1","dpi_gps1","dpi_gps2","dpi_gps3","dpi_gs","dpi_gvs","dpi_housesys","dpi_legelec","dpi_lipc","dpi_maj","dpi_mdmh","dpi_mt","dpi_nogp","dpi_nogps","dpi_noop","dpi_noops","dpi_nos","dpi_numul","dpi_plurality","dpi_pr","dpi_seats","dpi_slop1","dpi_slop2","dpi_system","dpi_yct","dpi_yio","dr_ig","dr_pg","dr_sg","epi_chmort","epi_epi","epi_forcov","epi_pacov","epi_pops","fao_fpic","ffp_fsi","fh_aor","fh_cl","fh_ep","fh_feb","fh_fog","fh_fotpa5","fh_fotpb5","fh_fotpc5","fh_fotpsc5","fh_fotpst5","fh_ipolity2","fh_pair","fh_polity2","fh_ppp","fh_pr","fh_rol","fh_status","gd_ptss","h_f","h_j","h_l1","h_l2","h_polcon3","h_polcon5","hf_business","hf_corrupt","hf_efiscore","hf_financ","hf_fiscal","hf_govt","hf_invest","hf_labor","hf_monetary","hf_prights","hf_trade","ht_colonial","ht_region","ht_region2","ht_regtype","ht_regtype1","ihme_nm","ihme_pnm","ipu_w_lower","lp_catho80","lp_lat_abst","lp_legor","lp_muslim80","lp_no_cpm80","lp_protmg80","p_autoc","p_democ","p_durable","p_flag","p_fragment","p_parcomp","p_parreg","p_polity","p_polity2","p_xconst","p_xrcomp","p_xropen","p_xrreg","pwt_csg","pwt_er","pwt_gsg","pwt_isg","pwt_openc","pwt_openk","pwt_pop","pwt_rgdpch","r_elf85","ross_gas_exp","ross_gas_netexp","ross_gas_netexpc","ross_gas_price","ross_gas_prod","ross_gas_value","ross_oil_exp","ross_oil_netexp","ross_oil_netexpc","ross_oil_price","ross_oil_prod","ross_oil_value","rsf_pfi","ti_cpi","ti_cpi_max","ti_cpi_min","ti_cpi_sd","undp_hdi","unna_er","unna_gdp","unna_pop","van_comp","van_index","van_part","wbgi_cce","wbgi_ccn","wbgi_ccs","wbgi_gee","wbgi_gen","wbgi_ges","wbgi_pse","wbgi_psn","wbgi_pss","wbgi_rle","wbgi_rln","wbgi_rls","wbgi_rqe","wbgi_rqn","wbgi_rqs","wbgi_vae.y","wbgi_van","wbgi_vas","wdi_area","wdi_ase","wdi_epc","wdi_eu","wdi_exp","wdi_fdi","wdi_fmort","wdi_fr","wdi_gce","wdi_gdp","wdi_gdpc","wdi_gdpcgr","wdi_gdpcu","wdi_gdpgr","wdi_gni","wdi_gnipc","wdi_hec","wdi_imp","wdi_infl","wdi_ise","wdi_lifexp","wdi_me","wdi_mort","wdi_pl","wdi_pop","wdi_prhe","wdi_puhe","wdi_sse","wdi_the","wdi_tot","wdi_trsb","wdi_ttr","wdi_urban","wdi_wip","wri_pa")),
    
    
    h3(helpText("Codebooks")),
    helpText(HTML("<ul>
                  <li><a href=\"http://www.qogdata.pol.gu.se/codebook/codebook_standard_15may13.pdf\">The QoG Standard Dataset 2013 </a></li>
                  </ul>")),
    
    helpText(HTML("<a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\">
                  <img alt=\"Creative Commons License\" style=\"border-width:0\" 
                  src=\"http://i.creativecommons.org/l/by-sa/3.0/88x31.png\" />
                  </a><br />This work is licensed under a <a rel=\"license\" 
                  href=\"http://creativecommons.org/licenses/by-sa/3.0/\">
                  Creative Commons Attribution-ShareAlike 3.0 Unported License</a>."))
    ),
  
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    
    plotOutput("scatter", width = "100%"),
    h3(helpText("Correlations")),
    htmlOutput("correlation")
    #     tabsetPanel(
    #       tabPanel("Summary", verbatimTextOutput("summary")), 
    #       tabPanel("Density plot", plotOutput("density")), 
    #       tabPanel("Boxplot", plotOutput("boxplot")), 
    #       tabPanel("Table", tableOutput("view"))
    #     )
  )
  #h3(textOutput("caption")), 
  #h3(helpText("Summary statistics")),
  #h3(helpText("Histogram")),
  #h3(helpText("First observations")),
  #tableOutput("view")
    ))
