// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"context"
	"errors"
	"net/http"
	"net/url"
)

type SearchReindexStatus struct {
	Status  string `json:"status"`
	Phase   string `json:"phase"`
	Total   int64  `json:"total"`
	Indexed int64  `json:"indexed"`
	Error   string `json:"error"`
}

var (
	ErrNoFullReindex = errors.New("adminapi: this Fluxer release can't rebuild every search index at once")
	ErrSearchOff     = errors.New("adminapi: search is turned off")
)

func (c *Client) ReindexSearch(ctx context.Context) (string, error) {
	var resp struct {
		JobID string `json:"job_id"`
	}
	err := c.Call(ctx, http.MethodPost, "/admin/search/indexes/all/refreshes", nil, map[string]any{}, &resp)
	var api *APIError
	switch {
	case errors.As(err, &api) && api.Status == http.StatusBadRequest:
		return "", ErrNoFullReindex
	case errors.As(err, &api) && api.Code == "FEATURE_TEMPORARILY_DISABLED":
		return "", ErrSearchOff
	case err != nil:
		return "", err
	}
	return resp.JobID, nil
}

func (c *Client) SearchReindexStatus(ctx context.Context, jobID string) (*SearchReindexStatus, error) {
	var resp SearchReindexStatus
	if err := c.Call(ctx, http.MethodGet, "/admin/search/index-refreshes/"+url.PathEscape(jobID), nil, nil, &resp); err != nil {
		return nil, err
	}
	return &resp, nil
}
