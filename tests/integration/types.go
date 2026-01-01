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

import (
	"embed"
	"encoding/base64"
	"fmt"
	"testing"
	"time"
)

type registerRequest struct {
	Email       string  `json:"email"`
	Username    string  `json:"username"`
	GlobalName  string  `json:"global_name"`
	Password    string  `json:"password"`
	DateOfBirth string  `json:"date_of_birth"`
	Consent     bool    `json:"consent"`
	BetaCode    *string `json:"beta_code,omitempty"`
	InviteCode  *string `json:"invite_code,omitempty"`
}

type registerResponse struct {
	UserID              string `json:"user_id"`
	Token               string `json:"token"`
	PendingVerification *bool  `json:"pending_verification,omitempty"`
}

type loginRequest struct {
	Email      string  `json:"email"`
	Password   string  `json:"password"`
	InviteCode *string `json:"invite_code,omitempty"`
}

type loginResponse struct {
	MFA                 bool   `json:"mfa"`
	UserID              string `json:"user_id"`
	Token               string `json:"token"`
	PendingVerification *bool  `json:"pending_verification,omitempty"`
	Ticket              string `json:"ticket,omitempty"`
	SMS                 bool   `json:"sms,omitempty"`
	TOTP                bool   `json:"totp,omitempty"`
	WebAuthn            bool   `json:"webauthn,omitempty"`
}

type userPrivateResponse struct {
	ID                         string                              `json:"id"`
	Email                      string                              `json:"email"`
	Phone                      *string                             `json:"phone"`
	Username                   string                              `json:"username"`
	Discriminator              string                              `json:"discriminator"`
	GlobalName                 string                              `json:"global_name"`
	Bio                        string                              `json:"bio"`
	Verified                   bool                                `json:"verified"`
	MfaEnabled                 bool                                `json:"mfa_enabled"`
	AuthenticatorTypes         []int                               `json:"authenticator_types"`
	PremiumType                int                                 `json:"premium_type"`
	PremiumUntil               string                              `json:"premium_until"`
	PremiumWillCancel          bool                                `json:"premium_will_cancel"`
	PremiumBillingCycle        string                              `json:"premium_billing_cycle"`
	PremiumLifetimeSequence    *int                                `json:"premium_lifetime_sequence"`
	PremiumPurchaseDisabled    bool                                `json:"premium_purchase_disabled"`
	PasswordLastChangedAt      *string                             `json:"password_last_changed_at"`
	PendingBulkMessageDeletion *pendingBulkMessageDeletionResponse `json:"pending_bulk_message_deletion,omitempty"`
}

type pendingBulkMessageDeletionResponse struct {
	ScheduledAt  string `json:"scheduled_at"`
	ChannelCount int    `json:"channel_count"`
	MessageCount int    `json:"message_count"`
}

type testEmail struct {
	To        string            `json:"to"`
	Subject   string            `json:"subject"`
	Type      string            `json:"type"`
	Timestamp time.Time         `json:"timestamp"`
	Metadata  map[string]string `json:"metadata"`
}

type testEmailListResponse struct {
	Emails []testEmail `json:"emails"`
}

type GiftCodeMetadataResponse struct {
	Code           string       `json:"code"`
	DurationMonths int          `json:"duration_months"`
	CreatedAt      string       `json:"created_at"`
	CreatedBy      userPartial  `json:"created_by"`
	RedeemedAt     *string      `json:"redeemed_at"`
	RedeemedBy     *userPartial `json:"redeemed_by"`
}

//go:embed fixtures/*
var fixturesFS embed.FS

type emojiResponse struct {
	ID       string `json:"id"`
	Name     string `json:"name"`
	Animated bool   `json:"animated"`
}

type rpcChannelResponse struct {
	ID            string `json:"id"`
	Type          int    `json:"type"`
	LastMessageID string `json:"last_message_id"`
}

type ipAuthSeedPayload struct {
	Ticket         string
	Token          string
	UserID         string
	Email          string
	Username       string
	ClientIP       string
	UserAgent      string
	ClientLocation string
	Platform       *string
	ResendUsed     bool
	InviteCode     *string
	CreatedAt      time.Time
	TTLSeconds     int
}

func loadFixtureAsDataURL(t testing.TB, filename, mimeType string) string {
	t.Helper()
	data, err := fixturesFS.ReadFile(fmt.Sprintf("fixtures/%s", filename))
	if err != nil {
		t.Fatalf("failed to read fixture %s: %v", filename, err)
	}
	encoded := base64.StdEncoding.EncodeToString(data)
	return fmt.Sprintf("data:%s;base64,%s", mimeType, encoded)
}
