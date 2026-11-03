// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {canonicalizeIpAddress} from '@fluxer/ip_utils/src/IpAddress';

interface IpSighting {
	ip: string;
	seenAt: Date | null;
}

interface RecentUserIp {
	ip: string;
	lastSeenAt: Date;
}

async function collectSightings(userRepository: UserRepository, user: User): Promise<Array<IpSighting>> {
	const sightings: Array<IpSighting> = [];
	if (user.lastActiveIp) {
		sightings.push({ip: user.lastActiveIp, seenAt: user.lastActiveAt});
	}
	try {
		for (const {ip} of await userRepository.getAuthorizedIps(user.id)) {
			if (ip) sightings.push({ip, seenAt: null});
		}
	} catch (error) {
		Logger.warn({error, userId: user.id.toString()}, 'Failed to list the authorized IPs of a user');
	}
	try {
		for (const session of await userRepository.listAuthSessions(user.id)) {
			if (session.clientIp) sightings.push({ip: session.clientIp, seenAt: session.approximateLastUsedAt});
		}
	} catch (error) {
		Logger.warn({error, userId: user.id.toString()}, 'Failed to list the auth sessions of a user');
	}
	try {
		for (const tombstone of await userRepository.listAuthSessionTombstones(user.id)) {
			if (tombstone.clientIp) sightings.push({ip: tombstone.clientIp, seenAt: tombstone.approximateLastUsedAt});
		}
	} catch (error) {
		Logger.warn({error, userId: user.id.toString()}, 'Failed to list the auth session tombstones of a user');
	}
	return sightings;
}

export async function collectKnownUserIps(userRepository: UserRepository, user: User): Promise<ReadonlySet<string>> {
	return new Set((await collectSightings(userRepository, user)).map((sighting) => sighting.ip));
}

export async function listRecentUserIps(
	userRepository: UserRepository,
	user: User,
	since: Date,
): Promise<Array<RecentUserIp>> {
	const latest = new Map<string, Date>();
	for (const {ip, seenAt} of await collectSightings(userRepository, user)) {
		const canonical = canonicalizeIpAddress(ip);
		if (!canonical || !seenAt || seenAt.getTime() < since.getTime()) continue;
		const previous = latest.get(canonical);
		if (!previous || previous.getTime() < seenAt.getTime()) {
			latest.set(canonical, seenAt);
		}
	}
	return [...latest]
		.map(([ip, lastSeenAt]) => ({ip, lastSeenAt}))
		.sort((left, right) => right.lastSeenAt.getTime() - left.lastSeenAt.getTime());
}
