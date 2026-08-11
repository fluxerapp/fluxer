// SPDX-License-Identifier: AGPL-3.0-or-later

import Bowser from 'bowser';
import {Logger} from '../Logger';

interface UserAgentInfo {
	clientOs: string;
	detectedPlatform: string;
}

const UNKNOWN_LABEL = 'Unknown';

function formatName(name?: string | null): string {
	const normalized = name?.trim();
	return normalized || UNKNOWN_LABEL;
}

function parseUserAgentSafe(userAgentRaw: string): UserAgentInfo {
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
