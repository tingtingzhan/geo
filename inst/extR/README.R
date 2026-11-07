
# internal data used by the package

# Shift+Command+S to source

rm(list = ls(all.names = TRUE))

source('./inst/extR/datahub.R')
source('./inst/extR/ip2location.R')
source('./inst/extR/plotly.R')
source('./inst/extR/tayljordan.R')
# EPSG:4326 is the standard authority code for the WGS84 (World Geodetic System 1984)

if (FALSE) {
  object.size(airports_datahub)
  object.size(airports_ip2location)
  object.size(ports_tayljordan)
}

# all 'sf' objects
save.image(file = './R/sysdata.rda', compress = 'xz')

rm(list = ls(all.names = TRUE))


# Google "R plotly add_segments hoverinfo" provides working code by AI.

# Google "R plotly add_segments hoverinfo along the segment" provides AI responses below:
# In plotly, the add_segments() function creates segments using a single start point and a single end point. Because of this underlying logic, the hover trigger only fires at the exact coordinate endpoints, not while traversing along the segment body.
