// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {BadgeTypes} from '@fluxer/constants/src/BadgeConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {BadgeResponse} from '@fluxer/schema/src/domains/badge/BadgeSchemas';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const SCAN_BATCH_SIZE = 500;
const ASSIGN_CONCURRENCY = 25;
const ICON_FETCH_TIMEOUT_MS = 30000;

interface LegacyBadge {
	flag: bigint;
	name: string;
	icon: string;
	path: string;
}

const LEGACY_BADGES: ReadonlyArray<LegacyBadge> = [
	{flag: UserFlags.PARTNER, name: 'Partner', icon: 'partner.svg', path: 'partners'},
	{flag: UserFlags.BUG_HUNTER, name: 'Bug Hunter', icon: 'bug-hunter.svg', path: 'help/report-bug'},
];

async function fetchLegacyIcon(fileName: string): Promise<string> {
	const response = await fetch(`${Config.endpoints.staticCdn}/badges/${fileName}`, {
		signal: AbortSignal.timeout(ICON_FETCH_TIMEOUT_MS),
	});
	if (!response.ok) {
		throw new Error(`Failed to fetch legacy badge icon ${fileName}: ${response.status}`);
	}
	return response.text();
}

async function scanLegacyBadgeHolders(): Promise<Map<LegacyBadge, Array<User>>> {
	const {userRepository} = getWorkerDependencies();
	const holders = new Map(LEGACY_BADGES.map((badge) => [badge, [] as Array<User>]));
	let pageState: string | null = null;
	do {
		const page = await userRepository.scanAllUsersPage(SCAN_BATCH_SIZE, pageState);
		pageState = page.pageState;
		for (const user of page.users) {
			for (const badge of LEGACY_BADGES) {
				if ((user.flags & badge.flag) !== 0n) holders.get(badge)!.push(user);
			}
		}
	} while (pageState);
	return holders;
}

const migrateLegacyBadges: WorkerTaskHandler = async (_payload, helpers) => {
	const instanceConfigRepository = getInstanceConfigRepository();
	if (await instanceConfigRepository.areLegacyBadgesMigrated()) return;
	const {userRepository, snowflakeService, gatewayService} = getWorkerDependencies();
	const holders = new Map([...(await scanLegacyBadgeHolders())].filter(([, users]) => users.length > 0));
	const {badges: existing} = await instanceConfigRepository.getBadgeConfig();
	const {product_name: productName} = (await instanceConfigRepository.getAppPublicConfig()).branding;
	const badgeIds = new Map<LegacyBadge, string>();
	const created: Array<BadgeResponse> = [];
	for (const badge of holders.keys()) {
		const match = existing.find((entry) => entry.type === BadgeTypes.USER && entry.name === badge.name);
		if (match) {
			badgeIds.set(badge, match.id);
			continue;
		}
		const id = (await snowflakeService.generate()).toString();
		badgeIds.set(badge, id);
		created.push({
			id,
			type: BadgeTypes.USER,
			name: badge.name,
			tooltip: `${productName} ${badge.name}`,
			icon: await fetchLegacyIcon(badge.icon),
			url: Config.endpoints.marketing ? `${Config.endpoints.marketing}/${badge.path}` : null,
			position: existing.length + created.length,
		});
	}
	if (created.length > 0) {
		const config = await instanceConfigRepository.updateBadgeConfig((current) => ({
			...current,
			badges: [...current.badges, ...created.filter((badge) => !current.badges.some((entry) => entry.id === badge.id))],
		}));
		await gatewayService.broadcastDispatch({event: 'BADGES_UPDATE', data: {version: config.version, badges: created}});
	}
	const assignments = new Map<User, Set<bigint>>();
	for (const [badge, users] of holders) {
		for (const user of users) {
			const ids = assignments.get(user) ?? new Set(user.badgeIds);
			ids.add(BigInt(badgeIds.get(badge)!));
			assignments.set(user, ids);
		}
	}
	await mapWithConcurrency([...assignments], ASSIGN_CONCURRENCY, ([user, ids]) =>
		userRepository.patchUpsert(user.id, {badge_ids: ids}, user.toRow()),
	);
	await instanceConfigRepository.markLegacyBadgesMigrated();
	helpers.logger.info(
		{createdBadges: created.length, assignedUsers: assignments.size},
		'Migrated legacy user flag badges',
	);
};

export default migrateLegacyBadges;
