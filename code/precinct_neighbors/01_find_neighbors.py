# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# find_neighbors.py
# Created on: 2020-02-24 09:31:41.00000
#   (generated by ArcGIS/ModelBuilder)
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

wd = "D:/florida_hurricane/"

# Local variables:
fl_2016_shp = "D:/florida_hurricane/raw_data/shapefiles/fl_2016/fl_2016.shp"
fl_2016_PolygonNeighbors1 = "C:/Users/morrisk/Documents/ArcGIS/Default.gdb/fl_2016_PolygonNeighbors1"
neighbors_txt = fl_2016_PolygonNeighbors1
temp = "D:/florida_hurricane/temp"

# Process: Polygon Neighbors
arcpy.PolygonNeighbors_analysis(fl_2016_shp, fl_2016_PolygonNeighbors1, "pct;county", "NO_AREA_OVERLAP", "BOTH_SIDES", "", "FEET", "SQUARE_FEET")

# Process: Table to Table
arcpy.TableToTable_conversion(fl_2016_PolygonNeighbors1, temp, "neighbors.txt", "", "src_pct \"src_pct\" true true false 50 Text 0 0 ,First,#,C:/Users/morrisk/Documents/ArcGIS/Default.gdb/fl_2016_PolygonNeighbors1,src_pct,-1,-1;nbr_pct \"nbr_pct\" true true false 50 Text 0 0 ,First,#,C:/Users/morrisk/Documents/ArcGIS/Default.gdb/fl_2016_PolygonNeighbors1,nbr_pct,-1,-1;src_county \"src_county\" true true false 50 Text 0 0 ,First,#,C:/Users/morrisk/Documents/ArcGIS/Default.gdb/fl_2016_PolygonNeighbors1,src_county,-1,-1;nbr_county \"nbr_county\" true true false 50 Text 0 0 ,First,#,C:/Users/morrisk/Documents/ArcGIS/Default.gdb/fl_2016_PolygonNeighbors1,nbr_county,-1,-1;LENGTH \"LENGTH\" true true false 8 Double 0 0 ,First,#,C:/Users/morrisk/Documents/ArcGIS/Default.gdb/fl_2016_PolygonNeighbors1,LENGTH,-1,-1;NODE_COUNT \"NODE_COUNT\" true true false 4 Long 0 0 ,First,#,C:/Users/morrisk/Documents/ArcGIS/Default.gdb/fl_2016_PolygonNeighbors1,NODE_COUNT,-1,-1", "")

