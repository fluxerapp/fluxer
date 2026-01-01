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
	"fmt"
	"net/http"
	"testing"
)

func TestDsaReportFlow(t *testing.T) {
	client := newTestClient(t)
	reporter := createTestAccount(t, client)
	author := createTestAccount(t, client)
	ensureSessionStarted(t, client, author.Token)

	guild := createGuild(t, client, author.Token, "DSA Report Guild")
	channel := createGuildChannel(t, client, author.Token, parseSnowflake(t, guild.ID), "general")
	msg := sendChannelMessage(t, client, author.Token, parseSnowflake(t, channel.ID), "Illegal content message")

	t.Run("MessageReport", func(t *testing.T) {
		ticket := issueDsaTicket(t, client, reporter.Email)
		payload := map[string]any{
			"ticket":                        ticket,
			"report_type":                   "message",
			"category":                      "illegal_activity",
			"reporter_full_legal_name":      "DSA Reporter",
			"reporter_country_of_residence": "SE",
			"additional_info":               "This message is clearly promoting illegal acts.",
			"message_link":                  fmt.Sprintf("https://fluxer.app/channels/%s/%s/%s", guild.ID, channel.ID, msg.ID),
		}

		resp, err := client.postJSON("/reports/dsa", payload)
		if err != nil {
			t.Fatalf("failed to submit message DSA report: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		var result map[string]any
		decodeJSONResponse(t, resp, &result)
		if _, ok := result["report_id"]; !ok {
			t.Fatal("expected report_id in DSA response")
		}
	})

	otherGuild := createGuild(t, client, author.Token, "DSA Guild Report")
	otherChannel := createGuildChannel(t, client, author.Token, parseSnowflake(t, otherGuild.ID), "general")
	invite := createChannelInvite(t, client, author.Token, parseSnowflake(t, otherChannel.ID))

	t.Run("UserReport", func(t *testing.T) {
		ticket := issueDsaTicket(t, client, reporter.Email)
		payload := map[string]any{
			"ticket":                        ticket,
			"report_type":                   "user",
			"category":                      "spam_account",
			"reporter_full_legal_name":      "DSA Reporter",
			"reporter_country_of_residence": "DE",
			"user_id":                       author.UserID,
			"additional_info":               "User is spamming illegal links.",
		}

		resp, err := client.postJSON("/reports/dsa", payload)
		if err != nil {
			t.Fatalf("failed to submit user DSA report: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		var result map[string]any
		decodeJSONResponse(t, resp, &result)
		if _, ok := result["report_id"]; !ok {
			t.Fatal("expected report_id in DSA response")
		}
	})

	t.Run("GuildReport", func(t *testing.T) {
		ticket := issueDsaTicket(t, client, reporter.Email)
		payload := map[string]any{
			"ticket":                        ticket,
			"report_type":                   "guild",
			"category":                      "malware_distribution",
			"reporter_full_legal_name":      "DSA Reporter",
			"reporter_country_of_residence": "FR",
			"guild_id":                      otherGuild.ID,
			"invite_code":                   invite.Code,
			"additional_info":               "Community distributes malware downloads.",
		}

		resp, err := client.postJSON("/reports/dsa", payload)
		if err != nil {
			t.Fatalf("failed to submit guild DSA report: %v", err)
		}
		defer resp.Body.Close()
		assertStatus(t, resp, http.StatusOK)

		var result map[string]any
		decodeJSONResponse(t, resp, &result)
		if _, ok := result["report_id"]; !ok {
			t.Fatal("expected report_id in DSA response")
		}
	})
}

func issueDsaTicket(t testing.TB, client *testClient, email string) string {
	sendDsaVerificationEmail(t, client, email)
	emailRecord := waitForEmail(t, client, "dsa_report_verification", email)
	code, ok := emailRecord.Metadata["code"]
	if !ok {
		t.Fatalf("missing verification code metadata for DSA email")
	}
	ticket := verifyDsaEmail(t, client, email, code)
	clearTestEmails(t, client)
	return ticket
}

func sendDsaVerificationEmail(t testing.TB, client *testClient, email string) {
	t.Helper()
	resp, err := client.postJSON("/reports/dsa/email/send", map[string]string{"email": email})
	if err != nil {
		t.Fatalf("failed to request DSA verification email: %v", err)
	}
	resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)
}

func verifyDsaEmail(t testing.TB, client *testClient, email, code string) string {
	t.Helper()
	resp, err := client.postJSON("/reports/dsa/email/verify", map[string]string{"email": email, "code": code})
	if err != nil {
		t.Fatalf("failed to verify DSA email: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var payload struct {
		Ticket string `json:"ticket"`
	}
	decodeJSONResponse(t, resp, &payload)
	if payload.Ticket == "" {
		t.Fatal("expected ticket from DSA verify")
	}
	return payload.Ticket
}
