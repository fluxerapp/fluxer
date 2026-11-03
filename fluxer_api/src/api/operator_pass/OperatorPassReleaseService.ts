// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {Db} from '@app/api/database/CassandraTypes';
import type {OperatorPassReleaseRow, OperatorPassWallEntryRow} from '@app/api/database/types/OperatorPassTypes';
import {
	loadOperatorPassEntitlementState,
	resolveOperatorPassHolder,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {getOperatorPassReleaseListWindow} from '@app/api/operator_pass/OperatorPassPrices';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import type {OperatorPassWallEntryStore} from '@app/api/operator_pass/OperatorPassWallEntryStore';
import {isOperatorPassWallListed} from '@app/api/operator_pass/OperatorPassWallPresentation';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {OperatorPassReleaseAlreadyPublishedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassReleaseAlreadyPublishedError';
import {OperatorPassReleaseNotReadyError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassReleaseNotReadyError';
import {UnknownOperatorPassReleaseEntryError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassReleaseEntryError';
import type {OperatorPassReleaseResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

const RELEASE_NAME_SORT_LOCALE = 'en';

interface OperatorPassReleaseCandidate {
	entry: OperatorPassWallEntryRow;
	name: string;
}

interface OperatorPassReleasePreview {
	year: number;
	cutoffAt: Date;
	publishBy: Date;
	published: OperatorPassReleaseRow | null;
	candidates: Array<OperatorPassReleaseCandidate>;
}

interface OperatorPassReleaseFile {
	release: OperatorPassReleaseRow;
	file: string;
	tasks: Array<string>;
}

function buildOperatorPassReleaseFile(year: number, tag: string, names: ReadonlyArray<string>): string {
	return [`The Operators of ${year}`, '', `Released in ${tag}. Thank you.`, '', ...names, ''].join('\n');
}

export class OperatorPassReleaseService {
	constructor(
		private readonly repository: OperatorPassRepository,
		private readonly userRepository: UserRepository,
		private readonly entries: OperatorPassWallEntryStore,
	) {}

	async previewRelease(year: number): Promise<OperatorPassReleasePreview> {
		const window = getOperatorPassReleaseListWindow(Config.operatorPass.releaseLists, year);
		const published = await this.repository.findRelease(year);
		const candidates: Array<OperatorPassReleaseCandidate> = [];
		if (!published) {
			const now = operatorPassNow();
			for (const entry of await this.entries.scanAll()) {
				if (
					!isOperatorPassWallListed(entry) ||
					!entry.release_list_opt_in ||
					entry.hidden ||
					!entry.live_name ||
					entry.release_erased_at ||
					!entry.delivered_at ||
					entry.delivered_at.getTime() > window.cutoffAt.getTime()
				) {
					continue;
				}
				const [user, state] = await Promise.all([
					this.userRepository.findUnique(entry.user_id),
					loadOperatorPassEntitlementState(this.repository, entry.user_id),
				]);
				if (user && resolveOperatorPassHolder(state, now)) {
					candidates.push({entry, name: entry.live_name});
				}
			}
		}
		candidates.sort((leftCandidate, rightCandidate) => {
			const left = leftCandidate.name;
			const right = rightCandidate.name;
			return left.localeCompare(right, RELEASE_NAME_SORT_LOCALE, {sensitivity: 'base'}) || left.localeCompare(right);
		});
		return {year, cutoffAt: window.cutoffAt, publishBy: window.publishBy, published, candidates};
	}

	async publishRelease(params: {
		year: number;
		tag: string;
		releasedAt: Date;
		adminId: UserID;
	}): Promise<OperatorPassReleaseFile> {
		const now = operatorPassNow();
		const preview = await this.previewRelease(params.year);
		if (preview.published) {
			throw new OperatorPassReleaseAlreadyPublishedError();
		}
		if (now.getTime() < preview.cutoffAt.getTime()) {
			throw new OperatorPassReleaseNotReadyError();
		}
		const names = preview.candidates.map((candidate) => candidate.name);
		const release: OperatorPassReleaseRow = {
			year: params.year,
			tag: params.tag,
			released_at: params.releasedAt,
			published_at: now,
			published_by: params.adminId,
			cutoff_at: preview.cutoffAt,
			names,
			erased_names: null,
		};
		if (!(await this.repository.createRelease(release))) {
			throw new OperatorPassReleaseAlreadyPublishedError();
		}
		for (const candidate of preview.candidates) {
			await this.entries.mutate(candidate.entry.user_id, now, params.adminId, (current) => ({
				patch:
					(current.release_published_year ?? params.year) > params.year
						? {updated_at: Db.set(now)}
						: {
								release_published_year: Db.set(params.year),
								release_published_at: Db.set(params.releasedAt),
								updated_at: Db.set(now),
							},
				events: [{kind: 'release_published', name: candidate.name, releaseYear: params.year, releaseTag: params.tag}],
			}));
		}
		return {release, file: buildOperatorPassReleaseFile(params.year, params.tag, names), tasks: []};
	}

	async eraseFromRelease(params: {year: number; userId: UserID; adminId: UserID}): Promise<OperatorPassReleaseFile> {
		const release = await this.repository.findRelease(params.year);
		const entry = await this.repository.findWallEntry(params.userId);
		if (!release || !entry) {
			throw new UnknownOperatorPassReleaseEntryError();
		}
		const events = (await this.repository.listWallEvents(params.userId)).filter(
			(event) => event.release_year === params.year,
		);
		const erasedName = events.filter((event) => event.kind === 'release_published').at(-1)?.name;
		if (!erasedName || events.some((event) => event.kind === 'erased') || !release.names.includes(erasedName)) {
			throw new UnknownOperatorPassReleaseEntryError();
		}
		const index = release.names.indexOf(erasedName);
		const names = release.names.filter((_, position) => position !== index);
		const next: OperatorPassReleaseRow = {
			...release,
			names,
			erased_names: [...(release.erased_names ?? []), erasedName],
		};
		await this.repository.saveRelease(next);
		const now = operatorPassNow();
		await this.entries.mutateExisting(params.userId, now, params.adminId, (current) => ({
			patch: {release_erased_at: Db.set(current.release_erased_at ?? now), updated_at: Db.set(now)},
			events: [{kind: 'erased', name: erasedName, releaseYear: params.year, releaseTag: release.tag}],
		}));
		return {
			release: next,
			file: buildOperatorPassReleaseFile(params.year, release.tag, names),
			tasks: [
				`Remove "${erasedName}" from the release notes of ${release.tag}.`,
				`Commit the updated OPERATORS file without "${erasedName}" in the next change to the source code.`,
			],
		};
	}

	async latestRelease(): Promise<OperatorPassReleaseResponse | null> {
		const latest = (await this.repository.listReleases()).at(-1);
		if (!latest) {
			return null;
		}
		return {
			year: latest.year,
			tag: latest.tag,
			released_at: latest.released_at.toISOString(),
			names: latest.names,
		};
	}
}
