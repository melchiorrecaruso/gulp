{ Description: Messages unit.

  Copyright (C) 2014-2016 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit gulpmessages;

{$mode objfpc}
{$H+}

interface

const
  gebrokenarchive   = 'Archive is broken, try with the "fix" command (%s)';
  gewrongmarker     = 'Wrong marker value (%s)';
  gewrongflag       = 'Wrong flag value (%s)';
  gechecksum        = 'Mismatched checksum (%s)';
  geabsolutepath    = 'Absolute path, try with the "--forcepath" option (%s)';

  gereadarchive     = 'Stream is not a valid archive (%s)';
  gereadstream      = 'Unable to read stream (%s)';
  geduplicates      = 'Duplicates non allowed (%s)';

  gecreatepath      = 'Unable to create path "%s"';
  gesetuserid       = 'Unable to set userid for "%s"';
  gesetgroupid      = 'Unable to set groupid for "%s"';
  gesetmode         = 'Unable to set mode for "%s"';
  gesetattributes   = 'Unable to set attributes for "%s"';
  gesetdatetime     = 'Unable to set date for "%s"';
  gerestoreitem     = 'Unable to restore item "%s"';
  gerestorelink     = 'Unable to restore symlink "%s"';
  gerestoredir      = 'Unable to restore directory "%s"';
  gedeletefile      = 'Unable to delete file "%s"';
  gerenamefile      = 'Unable to rename file "%s"';
  geunsupported     = 'Unsupported type for "%s"';

  gmdeleteitem      = 'Delete "%s"';

  gmsync            = 'Syncing the content of "%s"';
  gmsyncitem        = 'Syncing "%s"';
  gmsyncfinish      = 'Finished (%u added bytes)';

  gmrestore         = 'Restore the content of "%s"';
  gmrestoreitem     = 'Restoring "%s"';
  gmrestorefinish   = 'Finished (%u extracted bytes)';

  gmcheck           = 'Checking the content of "%s"';
  gmcheckitem       = 'Checking "%s"';
  gmcheckfinish     = 'Finished (%u checked bytes)';

  gmfix             = 'Fixing the content of "%s"';
  gmfixitem         = 'Fixing "%s"';
  gmfixfinish       = 'Finished (%u removed bytes)';

  gmpurge           = 'Purging the content of "%s"';
  gmmoveitem        = 'Purging %s';
  gmpurgefinish     = 'Finished (%u removed bytes)';

  gmlist            = 'Listing the content of "%s"';
  gmlistfinish      = 'Finished (%u listed items)';
  gmlistlastversion = 'Lastest version %u';

implementation

end.

