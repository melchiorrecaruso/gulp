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
  gesetid           = 'Unable to set user/group id for "%s"';
  gesetmode         = 'Unable to set mode for "%s"';
  gesetattributes   = 'Unable to set attributes for "%s"';
  gesetdatetime     = 'Unable to set date for "%s"';
  gerestoreitem     = 'Unable to restore item "%s"';
  gedeletefile      = 'Unable to delete file "%s"';
  gerenamefile      = 'Unable to rename file "%s"';

  gmdeleteitems     = '%sDelete items...          ';

  gmsync            = 'Sync the content of "%s" %s';
  gmscanningarchive = '%sScanning archive...      ';
  gmscanningfs      = '%sScanning filesystem...   ';
  gmsyncitems       = '%sSyncing items...         ';
  gmsyncfinish      = '%sFinished (%u added bytes) %s';

  gmrestore         = 'Restore the content of "%s" %s';
  gmrestoreitems    = '%sRestoring items...          ';
  gmrestorefinish   = '%sFinished (%u extracted bytes) %s';

  gmcheck           = 'Check the content of "%s" %s';
  gmcheckitems      = '%sChecking items...         ';
  gmcheckfinish     = '%sFinished (%u checked bytes) %s';

  gmfix             = 'Fix the content of "%s" %s';
  gmfixitems        = '%sFixing items...         ';
  gmfixfinish       = '%sFinished (%u removed bytes) %s';

  gmpurge           = 'Purge the content of "%s" %s';
  gmmoveitems       = '%sMoving items...           ';
  gmpurgefinish     = '%sFinished (%u removed bytes) %s';

  gmlist            = 'List the content of "%s" %s';
  gmlistitems       = '%sListing items...       %s';
  gmlistfinish      = 'Finished (%u listed items) %s';
  gmlistlastversion = 'Lastest version %u %s';

implementation

end.

