library(dplyr)
library(data.table)
library(tidyr)
'%notin%' <- Negate('%in%')

f <- fread("data/combined.fish.reference.may23.pcr.sin.pga.tax.fix.uni.cln.tsv")

length(unique(f$species)) # 15630

unique(f$class) # 15630

length(unique(f[f$class %in% c("Actinopteri", "Hyperoartia", "Chondrichthyes", "Cladistia"),]$species))
# 8279 unique species
length(f[f$class %in% c("Actinopteri", "Hyperoartia", "Chondrichthyes", "Cladistia"),]$species)
# 12893

length(unique(f$family)) # 957

length(unique(f$order))
table(f$order) # 152

unique(f$class)
length(unique(f$class)) # 9

# import list of NY, NJ, and PA "local" (and introduced to US) fish species
loc <- read.csv("data/local_fish_list.csv") %>%
  select(1:3) %>%
  # remove 4 out of range species from the Schroeter paper
  filter(Scientific.Name %notin% c("Scaphirhynchus albus",
                                   "Scaphirhynchus suttkusi",
                                   "Cyprinella leedsi",
                                   "Thymallus arcticus")) %>%
  mutate(species = gsub(Scientific.Name, pattern = " ", replacement = "_")) %>%
  # change some misspellings (or conform to NCBI spelling)
  mutate(species = case_when(species == "Anchoa_michilli" ~ "Anchoa_mitchilli",
                             species == "Moxostoma_duquesneii" ~ "Moxostoma_duquesnii",
                             species == "Moxostoma_duquesnei" ~ "Moxostoma duquesnii",
                             species == "Notropis_buccatus" ~ "Notropis_buccata",
                             species == "Percina_oxyrhyncha" ~ "Percina_oxyrhynchus",
                             species == "Phoxinus_eos" ~ "Chrosomus_eos",
                             species == "Phoxinus_neogaeus" ~ "Chrosomus_neogaeus",
                             species == "Rubricatochromis bimaculatus" ~ "Rubricatochromis_bimaculatus",
                             species == "Lethenteron_appendix" ~ "Lampetra_appendix",
                             TRUE ~ species)) %>%
  # remove redundant typos
  filter(species %notin% c("Cottus_bairdiI", "Channa_Argus", "Acipenser_oxyrhynchus",
                           "Cottus_sp._cf._cognatus", "Esox_americanus_americanus",
                           "Esox_americanus_vermiculatus",
                           "Ichthyomyzon_bdellium_",
                           "Ichthyomyzon_greeleyi_",
                           "Myoxocephalus_thompsoni",
                           "Notropis_photogenus",
                           "Sander_glaucus",
                           "Sander_vitreus_glaucus",
                           "Hybognathus_regis",
                           "Margariscus_nachtriebi" # subspecies per NCBI
                           ))

loclist <- loc %>%
  select(species) %>%
  distinct()
                                   
# subset just NY, PA, NJ species plus all introduced fish in US and non-fish vertebrates
fl <- f %>%
  filter(species %in% loc$species)

# subset just 86 NJ species plus an additional 71 introduced fish in US and non-fish vertebrates
loc.njex <- loc %>%
  filter(state_list %in% c("NJ", "FishBase introduced"))
fnjex <- f %>%
  filter(species %in% loc.njex$species)

# check how many sequences of each "local db" fish species are in the reference database
loc_in_ref <- table(fl$species) %>% 
  as.data.frame() %>%
  rename(species = 1, numseq = 2)

loc_in_njex_ref <- table(fnjex$species) %>% 
  as.data.frame() %>%
  rename(species = 1, numseq = 2)
  
loc.seqs <- loc %>% 
  left_join(loc_in_ref, by = join_by(species)) %>%
  replace_na(list(numseq = 0))
# write.csv(loc.seqs, "data/local_or_exotic_reference_sequences.csv", row.names = F)

# just sequences from NJ or exotics
loc.seqs.njex <- loc.njex %>% 
  left_join(loc_in_njex_ref, by = join_by(species)) %>%
  replace_na(list(numseq = 0))
# write.csv(loc.seqs.njex, "data/local_or_exotic_reference_sequences_njex.csv", row.names = F)

# how many total species on our local list have a ref sequence in the database?
loc.seqs.uni <- distinct(select(loc.seqs, species, numseq))
sum(loc.seqs.uni$numseq==0) # 55 of 311 species on list have no ref sequence
test <- loc.seqs.uni[loc.seqs.uni$numseq==0,]

# how many total species on our VERY local NJEX list have a ref sequence in the database?
loc.seqs.njex.uni <- distinct(select(loc.seqs.njex, species, numseq)) # nrow = 157
sum(loc.seqs.njex.uni$numseq==0) # 25 of 157 species on list have no ref sequence
test <- loc.seqs.njex.uni[loc.seqs.njex.uni$numseq==0,]

# how many NJ species list aren't in NJ/PA/NY/exotics ref database?
nj.seqs <- loc.seqs[loc.seqs$state_list=="NJ",]
# write.csv(nj.seqs, "data/nj_reference_sequences.csv", row.names = F)
nj.seqs[nj.seqs$numseq==0,]$Common.Name

# how many NJ species list aren't in NJ/exotics ref database?
nj.seqs.njex <- loc.seqs.njex[loc.seqs.njex$state_list=="NJ",] # 86 NJ species total
# write.csv(nj.seqs, "data/nj_reference_sequences.csv", row.names = F)
nj.seqs.njex[nj.seqs.njex$numseq==0,]$Common.Name
# these 9 NJ species aren't on the list of sequences included in the refrerence database...
# "Mud Sunfish"            "Satinfin Shiner"        "Blackbanded Sunfish"   
# "Banded Sunfish"         "Swamp Darter"           "Banded Killifish"      
# "Comely Shiner"          "Swallowtail Shiner"    
# "Margined Madtom"   
nj.seqs.njex[nj.seqs.njex$numseq==0,]$species
# "Acantharchus_pomotis"*   "Cyprinella_analostana"  "Enneacanthus_chaetodon"* 
# "Enneacanthus_obesus"*    "Etheostoma_fusiforme"* "Fundulus_diaphanus"     
# "Notropis_amoenus"*       "Notropis_procne"*        "Noturus_insignis" 
# note: Satinfin Shiner, Margined Madtom, & Banded Killifish are in the broader BLAST database (can add to local one)
# another note: A. Brook Lamprey IS in the database, 
    # but it didn't make it in this time as the NJDEP website has an alternate scientific name
      # "Lethenteron appendix" instead of "Lampetra appendix"
      # I fixed this code, so it will be in the local db in future runs
# all 4 would have been picked up by BLAST
# of the other 6, all but Mud Sunfish have a congener in GenBank

# subset just NY/PA/NJ/exotic fish species plus all non-fish vertebrates
flv <- f %>%
  filter(species %in% loc$species) %>%
# add in the non-fish vertebrates
  bind_rows(filter(f, class %in% c("Mammalia", "Aves", "Lepidosauria", "Amphibia")))

# write TSV file
# write.table(flv, "data/NJ_fish_ref.tsv", sep = "\t", quote = F, row.names = F)

# subset just NJ/exotic fish species plus all non-fish vertebrates
fnjexv <- f %>%
  filter(species %in% loc.njex$species) %>%
  # add in the non-fish vertebrates
  bind_rows(filter(f, class %in% c("Mammalia", "Aves", "Lepidosauria", "Amphibia")))

# write TSV file
# write.table(fnjexv, "data/NJ_plus_exotics_only_fish_ref.tsv", sep = "\t", quote = F, row.names = F)

