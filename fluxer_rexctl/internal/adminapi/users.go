// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"
)

const (
	fluxerEpochMillis = 1420070400000
	userFlagDisabled  = uint64(1) << 38
	userFlagDeleted   = uint64(1) << 34
	listPageSize      = 100
)

type User struct {
	ID        string `json:"id"`
	Username  string `json:"username"`
	Email     string `json:"email,omitempty"`
	Admin     bool   `json:"admin"`
	Banned    bool   `json:"banned"`
	MFA       bool   `json:"mfa"`
	CreatedAt string `json:"created_at"`

	deletionFinished bool
}

func (u User) DeletionFinished() bool { return u.deletionFinished }

type UserDetail struct {
	User
	Disabled     bool     `json:"disabled"`
	Verified     bool     `json:"verified"`
	LastActiveAt *string  `json:"last_active_at"`
	ACLs         []string `json:"acls"`
	Sessions     int      `json:"sessions"`
}

var ErrDeletionPending = errors.New("adminapi: the account is already being deleted")

var ErrAlreadyDeleted = errors.New("adminapi: the account was already deleted")

type BanBlocksDeletionError struct{ Username string }

func (e *BanBlocksDeletionError) Error() string {
	return fmt.Sprintf("adminapi: the ban on %s can't become an account closure", e.Username)
}

type UserNotFoundError struct{ Ref string }

func (e *UserNotFoundError) Error() string {
	return fmt.Sprintf("adminapi: no account matches %q", e.Ref)
}

type AmbiguousUserError struct {
	Ref     string
	Matches []User
}

func (e *AmbiguousUserError) Error() string {
	return fmt.Sprintf("adminapi: %d accounts match %q", len(e.Matches), e.Ref)
}

func userPath(id string, rest ...string) string {
	p := "/admin/users/" + url.PathEscape(id)
	for _, r := range rest {
		p += "/" + r
	}
	return p
}

func toUser(u UserAdminResponseSchema) User {
	out := User{
		ID:        u.Id,
		Username:  u.Username,
		Admin:     len(u.Acls) > 0,
		Banned:    bannedNow(u.TempBannedUntil),
		MFA:       u.HasTotp || len(u.AuthenticatorTypes) > 0,
		CreatedAt: SnowflakeTime(u.Id),

		deletionFinished: deletionFinished(u),
	}
	if u.Email != nil {
		out.Email = *u.Email
	}
	return out
}

func toDetail(u UserAdminResponseSchema, sessions int) UserDetail {
	flags, _ := strconv.ParseUint(u.Flags, 10, 64)
	acls := u.Acls
	if acls == nil {
		acls = []string{}
	}
	return UserDetail{
		User:         toUser(u),
		Disabled:     flags&userFlagDisabled != 0,
		Verified:     u.EmailVerified,
		LastActiveAt: u.LastActiveAt,
		ACLs:         acls,
		Sessions:     sessions,
	}
}

func bannedNow(until *string) bool {
	if until == nil || *until == "" {
		return false
	}
	t, err := time.Parse(time.RFC3339, *until)
	return err != nil || t.After(time.Now())
}

func SnowflakeTime(id string) string {
	n, err := strconv.ParseUint(id, 10, 64)
	if err != nil {
		return ""
	}
	ms := int64(n>>22) + fluxerEpochMillis
	return time.UnixMilli(ms).UTC().Format(time.RFC3339)
}

func isDeleted(u UserAdminResponseSchema) bool {
	flags, _ := strconv.ParseUint(u.Flags, 10, 64)
	return flags&userFlagDeleted != 0
}

func deletionFinished(u UserAdminResponseSchema) bool {
	return isDeleted(u) && u.PendingDeletionAt == nil
}

func (c *Client) ListUsers(ctx context.Context, search string, admins bool) ([]User, error) {
	out := []User{}
	for offset := 0; ; offset += listPageSize {
		q := map[string]string{"limit": strconv.Itoa(listPageSize), "offset": strconv.Itoa(offset)}
		if search != "" {
			q["q"] = search
		}
		if admins && search == "" {
			q["acl"] = "*"
		}
		var resp SearchUsersResponse
		if err := c.Call(ctx, http.MethodGet, "/admin/users", q, nil, &resp); err != nil {
			return nil, err
		}
		for _, u := range resp.Users {
			if isDeleted(u) || (admins && len(u.Acls) == 0) {
				continue
			}
			out = append(out, toUser(u))
		}
		if len(resp.Users) < listPageSize {
			return out, nil
		}
	}
}

func (c *Client) getUser(ctx context.Context, id string) (*UserAdminResponseSchema, error) {
	var resp LookupUserResponse
	if err := c.Call(ctx, http.MethodGet, userPath(id), nil, nil, &resp); err != nil {
		if IsStatus(err, http.StatusNotFound) {
			return nil, &UserNotFoundError{Ref: id}
		}
		return nil, err
	}
	if len(resp.Users) == 0 {
		return nil, &UserNotFoundError{Ref: id}
	}
	return &resp.Users[0], nil
}

func (c *Client) resolve(ctx context.Context, ref string) (*UserAdminResponseSchema, error) {
	ref = strings.TrimSpace(strings.TrimPrefix(strings.TrimSpace(ref), "@"))
	if ref == "" {
		return nil, &UserNotFoundError{Ref: ref}
	}
	if _, err := strconv.ParseUint(ref, 10, 64); err == nil {
		u, err := c.getUser(ctx, ref)
		if err == nil {
			return u, nil
		}
		if _, notFound := err.(*UserNotFoundError); !notFound {
			return nil, err
		}
	}
	query := map[string]string{"limit": strconv.Itoa(listPageSize)}
	switch {
	case strings.Contains(ref, "@"):
		query["email"] = ref
	case strings.Contains(ref, "#"):
		query["resolve"] = ref
	default:
		query["q"] = ref
	}
	var resp SearchUsersResponse
	if err := c.Call(ctx, http.MethodGet, "/admin/users", query, nil, &resp); err != nil {
		return nil, err
	}
	var matches []UserAdminResponseSchema
	for _, u := range resp.Users {
		if isDeleted(u) {
			continue
		}
		if query["q"] != "" && !strings.EqualFold(u.Username, ref) {
			continue
		}
		matches = append(matches, u)
	}
	switch len(matches) {
	case 0:
		return nil, &UserNotFoundError{Ref: ref}
	case 1:
		return &matches[0], nil
	}
	amb := &AmbiguousUserError{Ref: ref}
	for _, m := range matches {
		amb.Matches = append(amb.Matches, toUser(m))
	}
	return nil, amb
}

func (c *Client) ResolveUser(ctx context.Context, ref string) (*User, error) {
	u, err := c.resolve(ctx, ref)
	if err != nil {
		return nil, err
	}
	out := toUser(*u)
	return &out, nil
}

func (c *Client) ShowUser(ctx context.Context, ref string) (*UserDetail, error) {
	u, err := c.resolve(ctx, ref)
	if err != nil {
		return nil, err
	}
	var sessions ListUserSessionsResponse
	if err := c.Call(ctx, http.MethodGet, userPath(u.Id, "sessions"), nil, nil, &sessions); err != nil {
		return nil, err
	}
	d := toDetail(*u, len(sessions.Sessions))
	return &d, nil
}

type CreatedUser struct {
	User      User
	ResetLink string
}

func (c *Client) CreateUser(ctx context.Context, username, email string, admin bool) (*CreatedUser, error) {
	req := AdminUserCreateRequest{Username: username}
	if email != "" {
		req.Email = &email
	}
	if admin {
		req.Admin = &admin
	}
	var resp AdminUserCreateResponse
	if err := c.Call(ctx, http.MethodPost, "/admin/users", nil, req, &resp); err != nil {
		return nil, err
	}
	return &CreatedUser{User: toUser(resp.User), ResetLink: resp.PasswordReset.Url}, nil
}

func (c *Client) ResetLink(ctx context.Context, id string) (string, string, error) {
	var resp AdminPasswordResetLinkResponse
	if err := c.Call(ctx, http.MethodPost, userPath(id, "password-reset-link"), nil, nil, &resp); err != nil {
		return "", "", err
	}
	return resp.Url, resp.ExpiresAt.UTC().Format(time.RFC3339), nil
}

func (c *Client) SignInLink(ctx context.Context, id string) (string, time.Time, error) {
	var resp AdminSignInLinkResponse
	if err := c.Call(ctx, http.MethodPost, userPath(id, "sign-in-link"), nil, nil, &resp); err != nil {
		return "", time.Time{}, err
	}
	return resp.Url, resp.ExpiresAt, nil
}

func (c *Client) SignOut(ctx context.Context, id string) (int, error) {
	var resp TerminateSessionsResponse
	if err := c.Call(ctx, http.MethodDelete, userPath(id, "sessions"), nil, nil, &resp); err != nil {
		return 0, err
	}
	return int(resp.TerminatedCount), nil
}

func (c *Client) DisableMFA(ctx context.Context, id string) error {
	return c.Call(ctx, http.MethodDelete, userPath(id, "mfa"), nil, nil, nil)
}

func (c *Client) SetAdmin(ctx context.Context, id string, admin bool) error {
	acls := []AdminAclType{}
	if admin {
		acls = append(acls, AdminAclType("*"))
	}
	return c.Call(ctx, http.MethodPut, userPath(id, "acls"), nil, AdminUserAclsRequest{Acls: acls}, nil)
}

func auditHeaders(reason string) map[string]string {
	return map[string]string{"X-Audit-Log-Reason": strings.Join(strings.Fields(reason), " ")}
}

func (c *Client) Ban(ctx context.Context, id string, reason BanReason, note string) error {
	statement := note
	if statement == "" {
		statement = reason.Label
	}
	req := AdminUserBanRequest{
		DurationHours: 0,
		Decision: EnforcementDecisionInput{
			ReasonCode: EnforcementReasonCodeType(reason.Code),
			Source:     OwnReview,
			Automation: EnforcementDecisionInputAutomationNone,
			Statement:  &statement,
		},
	}
	return c.CallWithHeaders(ctx, http.MethodPut, userPath(id, "ban"), nil, auditHeaders(statement), req, nil)
}

func (c *Client) Unban(ctx context.Context, id string) error {
	var list struct {
		Decisions []struct {
			DecisionID string `json:"decision_id"`
			Kind       string `json:"kind"`
			Status     string `json:"status"`
		} `json:"decisions"`
	}
	q := map[string]string{"subject_type": "user", "subject_id": id, "limit": "100"}
	if err := c.Call(ctx, http.MethodGet, "/admin/enforcement-decisions", q, nil, &list); err != nil {
		return err
	}
	ended := false
	for _, d := range list.Decisions {
		if d.Kind != "suspension" || d.Status != "active" {
			continue
		}
		statement := "Unbanned with rexctl"
		req := map[string]string{"outcome": "lifted", "statement": statement}
		if err := c.CallWithHeaders(ctx, http.MethodPost, "/admin/enforcement-decisions/"+url.PathEscape(d.DecisionID)+"/end", nil, auditHeaders(statement), req, nil); err != nil {
			return err
		}
		ended = true
	}
	if ended {
		return nil
	}
	return c.CallWithHeaders(ctx, http.MethodDelete, userPath(id, "ban"), nil, auditHeaders("Unbanned with rexctl"), nil, nil)
}

func housekeeping() (ModerationBasis, error) {
	var basis ModerationBasis
	err := basis.FromModerationBasis2(ModerationBasis2{Type: Housekeeping})
	return basis, err
}

func (c *Client) DeleteMessages(ctx context.Context, id, reason string) (int, error) {
	basis, err := housekeeping()
	if err != nil {
		return 0, err
	}
	var resp DeleteAllUserMessagesResponse
	q := map[string]string{"dry_run": "false"}
	if err := c.CallWithHeaders(ctx, http.MethodDelete, userPath(id, "messages"), q, auditHeaders(reason), AdminUserMessageDeleteRequest{Basis: &basis}, &resp); err != nil {
		return 0, err
	}
	return int(resp.MessageCount), nil
}

var closureRefusedReasons = map[EnforcementReasonCodeType]bool{
	EnforcementReasonCodeTypeCHILDSEXUALCONTENT:   true,
	EnforcementReasonCodeTypeCHILDSAFETYVIOLATION: true,
	EnforcementReasonCodeTypeVIOLENTEXTREMISM:     true,
	EnforcementReasonCodeTypeSELFHARMPROMOTION:    true,
	EnforcementReasonCodeTypeREPORTINGABUSE:       true,
}

func (c *Client) deletionBasis(ctx context.Context, u UserAdminResponseSchema) (ModerationBasis, error) {
	if u.EnforcementDecisionId == nil {
		return housekeeping()
	}
	var decision struct {
		ReasonCode EnforcementReasonCodeType `json:"reason_code"`
		Statement  *string                   `json:"statement"`
	}
	if err := c.Call(ctx, http.MethodGet, "/admin/enforcement-decisions/"+url.PathEscape(*u.EnforcementDecisionId), nil, nil, &decision); err != nil {
		return ModerationBasis{}, err
	}
	if closureRefusedReasons[decision.ReasonCode] {
		return ModerationBasis{}, &BanBlocksDeletionError{Username: u.Username}
	}
	statement := "Your account was deleted."
	for _, r := range BanReasons {
		if EnforcementReasonCodeType(r.Code) == decision.ReasonCode {
			statement = r.Label
		}
	}
	if decision.Statement != nil && *decision.Statement != "" {
		statement = *decision.Statement
	}
	var basis ModerationBasis
	err := basis.FromModerationBasis0(ModerationBasis0{
		Type: Enforcement,
		Decision: EnforcementDecisionInput{
			ReasonCode: decision.ReasonCode,
			Source:     OwnReview,
			Automation: EnforcementDecisionInputAutomationNone,
			Statement:  &statement,
		},
	})
	return basis, err
}

func (c *Client) DeleteUser(ctx context.Context, id string) error {
	u, err := c.getUser(ctx, id)
	if err != nil {
		return err
	}
	if deletionFinished(*u) {
		return ErrAlreadyDeleted
	}
	basis, err := c.deletionBasis(ctx, *u)
	if err != nil {
		return err
	}
	immediate := true
	req := AdminUserDeletionScheduleRequest{DaysUntilDeletion: 1, Basis: basis, Immediate: &immediate}
	if u.PendingDeletionAt != nil {
		at, err := time.Parse(time.RFC3339Nano, *u.PendingDeletionAt)
		if err != nil {
			return err
		}
		req.ReplacePendingDeletionAt = &at
	}
	err = c.CallWithHeaders(ctx, http.MethodPut, userPath(id, "deletion"), nil, auditHeaders("Deleted with rexctl"), req, nil)
	if IsStatus(err, http.StatusConflict) {
		return ErrDeletionPending
	}
	return err
}
