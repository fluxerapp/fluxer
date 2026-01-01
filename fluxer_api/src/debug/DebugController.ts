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

import type {HonoApp} from '~/App';
import {Config} from '~/Config';
import {DEFAULT_CC, extractClientIp, formatGeoipLocation, getCountryCodeDetailed} from '~/utils/IpUtils';

export const DebugController = (app: HonoApp) => {
	app.get('/_debug/geoip', async (ctx) => {
		const manualIp = ctx.req.query('ip')?.trim() || null;
		const headerIp = extractClientIp(ctx.req.raw);
		const chosenIp = manualIp || headerIp;

		let countryCode = DEFAULT_CC;
		let normalizedIp: string | null = null;
		let error: string | null = null;
		let reason: string | null = null;
		let geoipLocation: string | null = null;

		if (chosenIp) {
			try {
				const result = await getCountryCodeDetailed(chosenIp);
				countryCode = result.countryCode;
				normalizedIp = result.normalizedIp;
				reason = result.reason;
				geoipLocation = formatGeoipLocation(result);
			} catch (err) {
				error = (err as Error).message;
			}
		}

		return ctx.json({
			x_forwarded_for: ctx.req.header('x-forwarded-for') || null,
			ip: chosenIp,
			manual_ip: manualIp,
			extracted_ip: headerIp,
			country_code: countryCode,
			normalized_ip: normalizedIp,
			reason,
			geoip_host: Config.geoip.host || null,
			geoip_provider: Config.geoip.provider,
			geoip_location: geoipLocation,
			default_cc: DEFAULT_CC,
			error,
		});
	});
};
