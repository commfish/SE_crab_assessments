# K. palof 
# katie.palof@alaska.gov, updated 3-6-20/ 7-14-20

# Southeast RKC stock panel figures for assessment memo and RIR 

# Notes: historically these were in each survey_area.R file but that is problematic when making
#       changes for docs, etc. Now these are all together here. 

# load ----------
source('./code/functions.R')
cur_yr <- 2020

# Survey areas and associated figure calls:-------
# Juneau =============================

# confidental figure --------
panel_figure('Juneau', 2020, 'Juneau', 1, 0) # panel with all 3 figures
panel_figure('Juneau', cur_yr, 'Juneau', 2, 0) # male panel
panel_figure('Juneau', cur_yr, 'Juneau', 3, 0) # female panel

### presentation figure -----
legend_panel_figure_NC_PRES('Juneau', cur_yr, 'Juneau', 2, 0, 'Juneau Area', 0.65, 0.844)
  ]panel_figure_NC_PRES('Juneau', cur_yr, 'Juneau', 3, 0, 'Juneau Area')

# Excursion ==========================
# confidental figure 
panel_figure('Excursion', cur_yr, 'Excursion', 1, 0) # panel with all 3 figures
panel_figure('Excursion', cur_yr, 'Excursion', 2, 0) # male panel
panel_figure('Excursion', cur_yr, 'Excursion', 3, 0) # female panel

### NON CONF panel --------------
# **FIX ** not working
panel_figure_NC('Excursion', cur_yr, 'Excursion', 1, 0) # panel with all 3 figures
panel_figure_NC('Excursion',  cur_yr, 'Excursion', 2, 0)

### presentation figure -----
panel_figure_NC_PRES('Excursion', cur_yr, 'Excursion', 2, 0, 'Excursion Inlet')
panel_figure_NC_PRES('Excursion', cur_yr, 'Excursion', 3, 0, 'Excursion Inlet')

# Lynn Sisters =======================
panel_figure('LynnSisters', cur_yr, 'LynnSisters', 1, 0) # panel with all 3 figures
panel_figure('LynnSisters', cur_yr, 'LynnSisters', 2, 0) # male panel
panel_figure('LynnSisters', cur_yr, 'LynnSisters', 3, 0) # female panel

### presentation figure -----
panel_figure_NC_PRES('LynnSisters', cur_yr, 'LynnSisters', 2, 0, 'Lynn Sisters')
panel_figure_NC_PRES('LynnSisters', cur_yr, 'LynnSisters', 3, 0, 'Lynn Sisters')

# Peril ==============================
panel_figure('Peril', cur_yr, 'Deadman Reach', 1, 0) # panel with all 3 figures
panel_figure('Peril', cur_yr, 'Deadman Reach', 2, 0) # male panel
panel_figure('Peril', cur_yr, 'Deadman Reach', 3, 0) # female panel

### presentation figure -----
panel_figure_NC_PRES('Peril', cur_yr, 'Deadman Reach', 2, 0, 'Peril Strait')
panel_figure_NC_PRES('Peril', cur_yr, 'Deadman Reach', 3, 0, 'Peril Strait')

# Pybus ==============================
panel_figure('Pybus', cur_yr, 'Pybus', 1, 0) # panel with all 3 figures
panel_figure('Pybus', cur_yr, 'Pybus', 2, 0) # male panel
panel_figure('Pybus', cur_yr, 'Pybus', 3, 0) # female panel

### presentation figure -----
panel_figure_NC_PRES('Pybus', cur_yr, 'Pybus', 2, 2, 'Pybus Bay')
panel_figure_NC_PRES('Pybus', cur_yr, 'Pybus', 3, 0, 'Pybus Bay') #female panel

# Gambier  ===========================
panel_figure('Gambier', cur_yr, 'Gambier', 1, 0) # panel with all 3 figures
panel_figure('Gambier', cur_yr, 'Gambier', 2, 0) # male panel
panel_figure('Gambier', cur_yr, 'Gambier', 3, 0) # female panel

### presentation figure -----
panel_figure_NC_PRES('Gambier', cur_yr, 'Gambier', 2, 0, 'Gambier Bay')
panel_figure_NC_PRES('Gambier', cur_yr, 'Gambier', 3, 0, 'Gambier Bay')

# Seymour ============================
panel_figure('Seymour', cur_yr, 'Seymour Canal', 1, 1) # panel with all 3 figures
panel_figure('Seymour', cur_yr, 'Seymour Canal', 2, 1) # male panel
panel_figure('Seymour', cur_yr, 'Seymour Canal', 3, 1) # female panel
### presentation figure -----
panel_figure_NC_PRES('Seymour', cur_yr, 'Seymour Canal', 2, 1, "Seymour Canal")
panel_figure_NC_PRES('Seymour', cur_yr, 'Seymour Canal', 3, 1, "Seymour Canal")
