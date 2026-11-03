// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"context"
	"net/http"
	"net/url"
)

type VoiceRegion struct {
	VoiceRegionAdminResponse
	Servers []VoiceServerAdminResponse `json:"servers"`
}

func (c *Client) VoiceRegions(ctx context.Context) ([]VoiceRegion, error) {
	var resp struct {
		Regions []VoiceRegion `json:"regions"`
	}
	if err := c.Call(ctx, http.MethodGet, "/admin/voice/regions", map[string]string{"include_servers": "true"}, nil, &resp); err != nil {
		return nil, err
	}
	return resp.Regions, nil
}

func (c *Client) CreateVoiceRegion(ctx context.Context, req CreateVoiceRegionRequest) error {
	return c.Call(ctx, http.MethodPost, "/admin/voice/regions", nil, req, nil)
}

func voiceServerPath(region, server string) string {
	return "/admin/voice/regions/" + url.PathEscape(region) + "/servers/" + url.PathEscape(server)
}

func (c *Client) CreateVoiceServer(ctx context.Context, region string, req CreateVoiceServerRequestBody) (*VoiceServerAdminResponse, error) {
	var resp CreateVoiceServerResponse
	if err := c.Call(ctx, http.MethodPost, "/admin/voice/regions/"+url.PathEscape(region)+"/servers", nil, req, &resp); err != nil {
		return nil, err
	}
	return &resp.Server, nil
}

func (c *Client) UpdateVoiceServer(ctx context.Context, region, server string, req UpdateVoiceServerRequestBody) (*VoiceServerAdminResponse, error) {
	var resp UpdateVoiceServerResponse
	if err := c.Call(ctx, http.MethodPatch, voiceServerPath(region, server), nil, req, &resp); err != nil {
		return nil, err
	}
	return &resp.Server, nil
}

func (c *Client) DeleteVoiceServer(ctx context.Context, region, server string) error {
	return c.Call(ctx, http.MethodDelete, voiceServerPath(region, server), nil, nil, nil)
}

func (c *Client) VoiceTestToken(ctx context.Context, region, server string) (*CreateVoiceServerTestTokenResponse, error) {
	var resp CreateVoiceServerTestTokenResponse
	if err := c.Call(ctx, http.MethodPost, voiceServerPath(region, server)+"/test-token", nil, nil, &resp); err != nil {
		return nil, err
	}
	return &resp, nil
}

func (c *Client) CreateVoiceJoinToken(ctx context.Context, req CreateVoiceJoinTokenRequest) (*CreateVoiceJoinTokenResponse, error) {
	var resp CreateVoiceJoinTokenResponse
	if err := c.Call(ctx, http.MethodPost, "/admin/voice/join-tokens", nil, req, &resp); err != nil {
		return nil, err
	}
	return &resp, nil
}

func (c *Client) VoiceJoinTokens(ctx context.Context) ([]VoiceJoinTokenAdminResponse, error) {
	var resp ListVoiceJoinTokensResponse
	if err := c.Call(ctx, http.MethodGet, "/admin/voice/join-tokens", nil, nil, &resp); err != nil {
		return nil, err
	}
	return resp.Tokens, nil
}

func (c *Client) RevokeVoiceJoinToken(ctx context.Context, id string) error {
	return c.Call(ctx, http.MethodDelete, "/admin/voice/join-tokens/"+url.PathEscape(id), nil, nil, nil)
}
