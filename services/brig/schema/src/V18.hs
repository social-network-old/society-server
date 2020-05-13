-- This file is part of the Society Server implementation.
--

--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module V18
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 18 "Add prekeys" $ do
  void $
    schema'
      [r|
        create columnfamily if not exists clients
            ( user   uuid
            , client text
            , tstamp timestamp
            , type   int
            , label  text
            , primary key (user, client)
            );
       |]
  void $
    schema'
      [r|
        create columnfamily if not exists prekeys
            ( user   uuid
            , client text
            , key    int
            , data   text
            , primary key (user, client, key)
            );
       |]
