// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build windows

package envfile

import (
	"os"

	"golang.org/x/sys/windows"
)

func currentUser() (*windows.SID, error) {
	u, err := windows.GetCurrentProcessToken().GetTokenUser()
	if err != nil {
		return nil, err
	}
	return u.User.Sid.Copy()
}

func trustedSIDs() ([]*windows.SID, error) {
	user, err := currentUser()
	if err != nil {
		return nil, err
	}
	system, err := windows.CreateWellKnownSid(windows.WinLocalSystemSid)
	if err != nil {
		return nil, err
	}
	admins, err := windows.CreateWellKnownSid(windows.WinBuiltinAdministratorsSid)
	if err != nil {
		return nil, err
	}
	return []*windows.SID{user, system, admins}, nil
}

func restrictToOwner(path string) error {
	sids, err := trustedSIDs()
	if err != nil {
		return err
	}
	inherit := uint32(windows.NO_INHERITANCE)
	if info, err := os.Stat(path); err == nil && info.IsDir() {
		inherit = windows.SUB_CONTAINERS_AND_OBJECTS_INHERIT
	}
	entries := make([]windows.EXPLICIT_ACCESS, len(sids))
	for i, sid := range sids {
		kind := windows.TRUSTEE_TYPE(windows.TRUSTEE_IS_WELL_KNOWN_GROUP)
		if i == 0 {
			kind = windows.TRUSTEE_IS_USER
		}
		entries[i] = windows.EXPLICIT_ACCESS{
			AccessPermissions: windows.GENERIC_ALL,
			AccessMode:        windows.SET_ACCESS,
			Inheritance:       inherit,
			Trustee: windows.TRUSTEE{
				TrusteeForm:  windows.TRUSTEE_IS_SID,
				TrusteeType:  kind,
				TrusteeValue: windows.TrusteeValueFromSID(sid),
			},
		}
	}
	acl, err := windows.ACLFromEntries(entries, nil)
	if err != nil {
		return err
	}
	return windows.SetNamedSecurityInfo(path, windows.SE_FILE_OBJECT,
		windows.DACL_SECURITY_INFORMATION|windows.PROTECTED_DACL_SECURITY_INFORMATION, nil, nil, acl, nil)
}

func keepOwner(*os.File, os.FileInfo) {}

func syncDir(string) {}

func RestrictToOwner(path string) error {
	return restrictToOwner(path)
}
