-----------------------------------------------------------------------------
--
-- Module      :  Map
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module App.Map (
    Walker (..)
  , performLogic
  , mapA
) where

import App.Walker

mapA :: [[Char]]
mapA = ["################################"
      , "#                              #"
      , "##### ######################## #"
      , "# # ###       #   #          # #"
      , "# #     ##### ### # ######## # #"
      , "# ### ### # #  #  # #      # # #"
      , "#   # # # #   ###   # # ## # # #"
      , "# # ### # ## ######## # ##   # #"
      , "# # ### #  # #   #    #    ### #"
      , "# # # # ## # #   # ####### # # #"
      , "# #        # #   #     #     # #"
      , "# ######## # ## ###### # ### # #"
      , "# # #      #  # #   #  # # # # #"
      , "# # #### #### # # # # ## # # # #"
      , "#   #       # # # # #  #     # #"
      , "# ### ## #### # # #### ##### # #"
      , "# # ###  #  # # # #        # # #"
      , "# # ### ### # # # # # #### # # #"
      , "# #     ### # # # # # #    # # #"
      , "# # # # # # # # # # ######## # #"
      , "# # # #     #     #    #     # #"
      , "# ### ################ # # # # #"
      , "# #   #              # # #   # #"
      , "# # #### ############# # # # # #"
      , "# #                    #     # #"
      , "# # #### ############### # # # #"
      , "# #  # # #   #       ###     # #"
      , "# # ## # ### ### ### ### # ### #"
      , "# #          #   #   # # #   # #"
      , "# ################## # ### ### #"
      , "#                      #       #"
      , "################################"]

performLogic :: (Walker a) => a -> a
performLogic walker = walkerTurn walker mapA
