--****
-- == SVN Handling Routines
--
-- <<LEVELTOC level=2 depth=5>>
--


include std/get.e as get
include joy.e as joy


--**
-- Description:
-- Function extracts a version out a string such as "$Revision: 86 $"
-- A file that you want to keep track of its version should start in the following manner
-- <eucode>
--    include svn.e as svn
--    export constant file_version = extract_version( "$Revision$" )
-- </eucode>
-- 
-- or like this
-- 
-- <eucode>
--    include svn.e as svn
--    export constant file_version = extract_version( "$Revision: 777$" )
-- </eucode>
-- 
-- Use:
-- Now the file needs to have the SVN property "svn:keywords" set to "Revision" and not to '$Revision$' like the Tortoise SVN documentation indicates.  When this is done the $Revision$ or strings like $Revision: 777$ will be updated to the number indicated to the revision.  Extract version will convert this string into a number.
-- 
-- A program can have its svn version tracked as the maximum of the file_versions of all of the files it includes like this
-- 
-- <eucode>
-- include svn.e as svn
-- include joy.e as joy
-- include otherlongnamed.e as foo
-- include bar.e as bar
-- constant file_version = extract_version( "$Revision$" )
-- constant program_version = joy:max( file_version & svn:file_version & 
--                                     foo:file_version & 
--                                     bar:file_version & joy:file_version)
-- 
-- </eucode>
-- Both the include file and program file must be set with the svn property for substituting
-- the version number into strings like $Revision: 86$
-- 
export function extract_version( sequence s )
	s = joy:match_replace(  {"$Revision: ","$Revision","$"}, {"","",""}, s )
	s = get:value( s )
	if s[1] = GET_SUCCESS then
		return s[2]
	else
		return 0
	end if
end function

--**
-- A constant for describing the maximum svn version of this file and all files included in it.
export constant file_version =  extract_version( "$Revision: 86 $" )
