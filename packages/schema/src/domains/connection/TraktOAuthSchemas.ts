// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

export const TraktAuthorizeResponse = z.object({
	authorize_url: z.string().describe('The URL to redirect the user to for Trakt authorisation'),
});

export type TraktAuthorizeResponse = z.infer<typeof TraktAuthorizeResponse>;
