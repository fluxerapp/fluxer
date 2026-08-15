// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

export const GeoEntry = z.object({
	countryCode: z.string().describe('ISO 3166-1 alpha-2 country code'),
	regionCode: z
		.string()
		.nullable()
		.describe('ISO 3166-2 subdivision code, or null when the entry covers the whole country'),
});

export type GeoEntry = z.infer<typeof GeoEntry>;

export const GeolocationResponse = z.object({
	countryCode: z
		.string()
		.nullable()
		.describe('ISO 3166-1 alpha-2 country code resolved for the client, or null when it cannot be resolved'),
	regionCode: z
		.string()
		.nullable()
		.describe('ISO 3166-2 subdivision code resolved for the client, or null when it cannot be resolved'),
	latitude: z.string().nullable().describe('Approximate latitude of the client, or null when it cannot be resolved'),
	longitude: z.string().nullable().describe('Approximate longitude of the client, or null when it cannot be resolved'),
	ageRestrictedGeos: z.array(GeoEntry).describe('Locations where age restricted content requires an age check'),
	ageBlockedGeos: z.array(GeoEntry).describe('Locations where age restricted content is unavailable'),
});

export type GeolocationResponse = z.infer<typeof GeolocationResponse>;
