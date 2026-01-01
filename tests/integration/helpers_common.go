/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

package integration

// Constants for relationship types
const (
	relationshipFriend   = 1
	relationshipBlocked  = 2
	relationshipIncoming = 3
	relationshipOutgoing = 4
)

// Constants for endpoints
const (
	preloadMessagesEndpoint = "/users/@me/channels/messages/preload"
	messagesDeleteEndpoint  = "/users/@me/messages/delete"
)

type guildCreateResponse struct {
	ID             string   `json:"id"`
	SystemChannel  string   `json:"system_channel_id"`
	OwnerID        string   `json:"owner_id"`
	DefaultMessage int      `json:"default_message_notifications"`
	Features       []string `json:"features"`
}

type minimalChannelResponse struct {
	ID string `json:"id"`
}

type inviteResponse struct {
	Code string `json:"code"`
}

type messageResponse struct {
	ID        string `json:"id"`
	ChannelID string `json:"channel_id"`
}

type channelPinResponse struct {
	Message  messageResponse `json:"message"`
	PinnedAt string          `json:"pinned_at"`
}

type betaCodeEntry struct {
	Code       string  `json:"code"`
	RedeemedAt *string `json:"redeemed_at"`
}

type betaCodeListResponse struct {
	BetaCodes   []betaCodeEntry `json:"beta_codes"`
	Allowance   int             `json:"allowance"`
	NextResetAt *string         `json:"next_reset_at"`
}

type userNoteResponse struct {
	Note string `json:"note"`
}

type userSettingsResponse struct {
	Status                string `json:"status"`
	Theme                 string `json:"theme"`
	InlineAttachmentMedia bool   `json:"inline_attachment_media"`
	GifAutoPlay           bool   `json:"gif_auto_play"`
}

type mentionListResponse []messageResponse

type harvestStatusResponse struct {
	HarvestID            string  `json:"harvest_id"`
	RequestedAt          string  `json:"requested_at"`
	StartedAt            *string `json:"started_at"`
	CompletedAt          *string `json:"completed_at"`
	FailedAt             *string `json:"failed_at"`
	FileSize             *string `json:"file_size"`
	ProgressPercent      int     `json:"progress_percent"`
	ProgressStep         *string `json:"progress_step"`
	ErrorMessage         *string `json:"error_message"`
	DownloadURLExpiresAt *string `json:"download_url_expires_at"`
}

type harvestDownloadResponse struct {
	DownloadURL string `json:"downloadUrl"`
	ExpiresAt   string `json:"expiresAt"`
}

type relationshipResponse struct {
	ID   string      `json:"id"`
	Type int         `json:"type"`
	User userPartial `json:"user"`
}

type tagCheckResponse struct {
	Taken bool `json:"taken"`
}

type userPartial struct {
	ID            string `json:"id"`
	Username      string `json:"username"`
	Discriminator string `json:"discriminator"`
	GlobalName    string `json:"global_name"`
}

type userProfileEnvelope struct {
	User        userPartial `json:"user"`
	UserProfile struct {
		Bio string `json:"bio"`
	} `json:"user_profile"`
}

type userGuildSettingsResponse struct {
	GuildID          *string `json:"guild_id"`
	SuppressEveryone bool    `json:"suppress_everyone"`
}

type backupCodesResponse struct {
	BackupCodes []struct {
		Code string `json:"code"`
	} `json:"backup_codes"`
}

type mfaLoginResponse struct {
	Token string `json:"token"`
}

type phoneVerifyResponse struct {
	Phone      string `json:"phone"`
	Verified   bool   `json:"verified"`
	PhoneToken string `json:"phone_token"`
}

type webAuthnCredentialMetadata struct {
	ID   string `json:"id"`
	Name string `json:"name"`
}

type authSessionResponse struct {
	ID             string `json:"id"`
	ClientOS       string `json:"client_os"`
	ClientPlatform string `json:"client_platform"`
	ClientLocation string `json:"client_location"`
}

type testAccount struct {
	UserID   string
	Token    string
	Email    string
	Password string
}

type userSecurityFlagsPayload struct {
	SetFlags                    []string `json:"set_flags,omitempty"`
	ClearFlags                  []string `json:"clear_flags,omitempty"`
	SuspiciousActivityFlags     *int     `json:"suspicious_activity_flags,omitempty"`
	SuspiciousActivityFlagNames []string `json:"suspicious_activity_flag_names,omitempty"`
	PendingManualVerification   *bool    `json:"pending_manual_verification,omitempty"`
}

type registerOption func(*registerRequest)

type oauth2ApplicationResponse struct {
	ID           string           `json:"id"`
	Name         string           `json:"name"`
	RedirectURIs []string         `json:"redirect_uris"`
	ClientSecret string           `json:"client_secret,omitempty"`
	BotPublic    bool             `json:"bot_public"`
	Bot          *oauth2BotObject `json:"bot,omitempty"`
}

type oauth2BotObject struct {
	ID                 string `json:"id"`
	Username           string `json:"username,omitempty"`
	Discriminator      string `json:"discriminator,omitempty"`
	Avatar             string `json:"avatar,omitempty"`
	Token              string `json:"token,omitempty"`
	MFAEnabled         bool   `json:"mfa_enabled,omitempty"`
	AuthenticatorTypes []int  `json:"authenticator_types,omitempty"`
}

type oauth2TokenResponse struct {
	AccessToken  string `json:"access_token"`
	TokenType    string `json:"token_type"`
	ExpiresIn    int    `json:"expires_in"`
	RefreshToken string `json:"refresh_token,omitempty"`
	Scope        string `json:"scope,omitempty"`
}

type oauth2IntrospectionResponse struct {
	Active    bool   `json:"active"`
	Scope     string `json:"scope,omitempty"`
	ClientID  string `json:"client_id,omitempty"`
	Username  string `json:"username,omitempty"`
	TokenType string `json:"token_type,omitempty"`
	Exp       int64  `json:"exp,omitempty"`
	Iat       int64  `json:"iat,omitempty"`
	Sub       string `json:"sub,omitempty"`
}
