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

import Bowser from 'bowser';
import {Logger} from '~/Logger';

export interface UserAgentInfo {
	clientOs: string;
	detectedPlatform: string;
}

const UNKNOWN_LABEL = 'Unknown';

function formatName(name?: string | null): string {
	const normalized = name?.trim();
	return normalized || UNKNOWN_LABEL;
}

export function parseUserAgentSafe(userAgentRaw: string): UserAgentInfo {
	const ua = userAgentRaw.trim();
	if (!ua) return {clientOs: UNKNOWN_LABEL, detectedPlatform: UNKNOWN_LABEL};

	try {
		const parser = Bowser.getParser(ua);
		return {
			clientOs: formatName(parser.getOSName()),
			detectedPlatform: formatName(parser.getBrowserName()),
		};
	} catch (error) {
		Logger.warn({error}, 'Failed to parse user agent');
		return {clientOs: UNKNOWN_LABEL, detectedPlatform: UNKNOWN_LABEL};
	}
}

export function resolveSessionClientInfo(args: {userAgent: string | null; isDesktopClient: boolean | null}): {
	clientOs: string;
	clientPlatform: string;
} {
	const parsed = parseUserAgentSafe(args.userAgent ?? '');
	const clientPlatform = args.isDesktopClient ? 'Fluxer Desktop' : parsed.detectedPlatform;
	return {
		clientOs: parsed.clientOs,
		clientPlatform,
	};
}
