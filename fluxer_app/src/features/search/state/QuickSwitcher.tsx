// SPDX-License-Identifier: AGPL-3.0-or-later

import Guilds from '@app/features/guild/state/Guilds';
import {onLocaleChange} from '@app/features/i18n/utils/LocaleChangeListener';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch, {type SearchContext, type TransformedMember} from '@app/features/member/state/MemberSearch';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import {parseChannelUrl} from '@app/features/navigation/utils/DeepLinkUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import {buildCandidateSets} from '@app/features/search/state/QuickSwitcherCandidateBuilder';
import {getFirstSelectableIndex} from '@app/features/search/state/QuickSwitcherResultConverters';
import {
	generateDefaultResults,
	generateGeneralResults,
	generateQueryModeResults,
	type QuickSwitcherModeVariant,
	type QuickSwitcherSearchContext,
	resolveTransformedMember,
} from '@app/features/search/state/QuickSwitcherResultGenerators';
import {hasSameResultIdentity, resolveRecomputedSelectedIndex} from '@app/features/search/state/QuickSwitcherSelection';
import type {
	CandidateSets,
	ComputeResultsForQueryResult,
	LinkResult,
	QuickSwitcherExecutableResult,
	QuickSwitcherQueryMode,
	QuickSwitcherResult,
} from '@app/features/search/state/QuickSwitcherTypes';
import {MEMBER_SEARCH_LIMIT, QUICK_SWITCHER_OVERLAY_ID} from '@app/features/search/state/QuickSwitcherTypes';
import {
	createForwardSearchCandidates,
	type ForwardSearchCandidates,
} from '@app/features/search/utils/DestinationSearchSources';
import {getLoadedUnicodeConfusables, loadUnicodeConfusables} from '@app/features/search/utils/SearchTextMatching';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {makeAutoObservable, runInAction} from 'mobx';

const GO_TO_MESSAGE_DESCRIPTOR = msg({
	message: 'Go to message',
	comment: 'Short label in the quick switcher state. Keep it concise.',
});
const PREFIX_MODES: ReadonlyMap<string, QuickSwitcherQueryMode> = new Map([
	['@', QuickSwitcherResultTypes.USER],
	['#', QuickSwitcherResultTypes.TEXT_CHANNEL],
	['!', QuickSwitcherResultTypes.VOICE_CHANNEL],
	['*', QuickSwitcherResultTypes.GUILD],
]);
const ALL_USERS_PREFIX = '@@';
const NO_CONFUSABLES: ReadonlyMap<string, string> = new Map();
const QUICK_SWITCHER_I18N_MISSING_ERROR = 'QuickSwitcher i18n has not been set';

interface ParsedPrefix {
	readonly mode: QuickSwitcherQueryMode | null;
	readonly search: string;
	readonly variant: QuickSwitcherModeVariant;
}

function parsePrefix(query: string): ParsedPrefix {
	if (query.startsWith(ALL_USERS_PREFIX)) {
		return {
			mode: QuickSwitcherResultTypes.USER,
			search: query.slice(ALL_USERS_PREFIX.length).trim(),
			variant: 'all_users',
		};
	}
	const mode = PREFIX_MODES.get(query.charAt(0)) ?? null;
	return {mode, search: (mode == null ? query : query.slice(1)).trim(), variant: null};
}

type QuickSwitcherModalModule = typeof import('@app/features/search/components/quick_switcher/QuickSwitcherModal');

class QuickSwitcher {
	private logger = new Logger('QuickSwitcher');
	private candidateSets: CandidateSets | null = null;
	private searchSources: ForwardSearchCandidates | null = null;
	private confusables: ReadonlyMap<string, string> = getLoadedUnicodeConfusables() ?? NO_CONFUSABLES;
	private candidateWarmupCancel: (() => void) | null = null;
	private defaultResults: Array<QuickSwitcherResult> | null = null;
	private modalPreloadPromise: Promise<QuickSwitcherModalModule> | null = null;
	private modalPreloadCancel: (() => void) | null = null;
	isOpen = false;
	query = '';
	prefixMode: QuickSwitcherQueryMode | null = null;
	results: Array<QuickSwitcherResult> = [];
	selectedIndex = -1;
	private memberSearchContext: SearchContext | null = null;
	private memberFetchDebounceTimer: NodeJS.Timeout | null = null;
	private isFetchingMembersInBackground = false;
	private memberSearchResults: Array<GuildMember> = [];
	private i18n: I18n | null = null;

	constructor() {
		makeAutoObservable<
			this,
			| 'candidateSets'
			| 'searchSources'
			| 'confusables'
			| 'candidateWarmupCancel'
			| 'defaultResults'
			| 'logger'
			| 'modalPreloadCancel'
			| 'modalPreloadPromise'
		>(
			this,
			{
				candidateSets: false,
				searchSources: false,
				confusables: false,
				candidateWarmupCancel: false,
				defaultResults: false,
				logger: false,
				modalPreloadCancel: false,
				modalPreloadPromise: false,
			},
			{autoBind: true},
		);
	}

	setI18n(i18n: I18n): void {
		this.i18n = i18n;
		onLocaleChange(() => this.handleLocaleChange());
	}

	private handleLocaleChange(): void {
		this.invalidateCandidateSets();
		this.defaultResults = null;
		this.recomputeIfOpen({force: true, invalidateCandidates: false});
	}

	preloadModal(): void {
		if (MobileLayout.isMobileLayout() || this.modalPreloadPromise || this.modalPreloadCancel) {
			return;
		}
		this.modalPreloadCancel = this.scheduleDeferredWork(() => {
			this.modalPreloadCancel = null;
			void this.loadModal().catch((error) => {
				this.logger.error('Quick switcher modal preload failed', error);
			});
		}, 600);
	}

	private getI18n(): I18n {
		if (!this.i18n) {
			throw new Error(QUICK_SWITCHER_I18N_MISSING_ERROR);
		}
		return this.i18n;
	}

	getIsOpen(): boolean {
		return this.isOpen;
	}

	getResults(): ReadonlyArray<QuickSwitcherResult> {
		return this.results;
	}

	getSelectedResult(): QuickSwitcherExecutableResult | null {
		if (this.selectedIndex < 0 || this.selectedIndex >= this.results.length) {
			return null;
		}
		const result = this.results[this.selectedIndex];
		if (result.type === QuickSwitcherResultTypes.HEADER) {
			return null;
		}
		return result;
	}

	get isLoadingMemberResults(): boolean {
		return (
			this.prefixMode === QuickSwitcherResultTypes.USER &&
			this.query['slice'](1).trim().length > 0 &&
			(this.memberFetchDebounceTimer !== null || this.isFetchingMembersInBackground)
		);
	}

	findNextSelectableIndex(direction: 'up' | 'down', startIndex?: number): number {
		if (this.results.length === 0) return -1;
		let index = startIndex ?? this.selectedIndex;
		const step = direction === 'down' ? 1 : -1;
		for (let i = 0; i < this.results.length; i += 1) {
			index += step;
			if (index < 0) index = this.results.length - 1;
			if (index >= this.results.length) index = 0;
			if (this.results[index].type !== QuickSwitcherResultTypes.HEADER) {
				return index;
			}
		}
		return this.selectedIndex;
	}

	show(): void {
		if (this.isOpen) return;
		this.cancelCandidateWarmup();
		this.candidateSets = null;
		this.defaultResults = null;
		this.isOpen = true;
		this.query = '';
		this.prefixMode = null;
		if (!MobileLayout.isMobileLayout()) {
			void this.pushModal();
		}
		try {
			const {results, selectedIndex} = this.computeResultsForQuery('');
			this.results = results;
			this.selectedIndex = selectedIndex;
		} catch (error) {
			if (error instanceof Error && error.message === QUICK_SWITCHER_I18N_MISSING_ERROR) {
				throw error;
			}
			this.logger.error('Quick switcher failed to precompute results', error);
			this.results = [];
			this.selectedIndex = -1;
		}
		this.scheduleCandidateWarmup();
		this.ensureConfusables();
	}

	private ensureConfusables(): void {
		if (this.confusables !== NO_CONFUSABLES) return;
		loadUnicodeConfusables().then(
			(loaded) => {
				this.confusables = loaded;
				this.recomputeIfOpen({invalidateCandidates: false});
			},
			(error: unknown) => this.logger.warn('Failed to load Unicode confusables', error),
		);
	}

	private loadModal(): Promise<QuickSwitcherModalModule> {
		this.modalPreloadPromise ??= loadLazyModule(
			() => import('@app/features/search/components/quick_switcher/QuickSwitcherModal'),
		).catch((error) => {
			this.modalPreloadPromise = null;
			throw error;
		});
		return this.modalPreloadPromise;
	}

	private async pushModal(): Promise<void> {
		let modalModule: QuickSwitcherModalModule;
		try {
			modalModule = await this.loadModal();
		} catch (error) {
			if (this.isOpen && !MobileLayout.isMobileLayout()) {
				this.logger.error('Quick switcher modal failed to load', error);
				this.hide();
			}
			return;
		}
		if (!this.isOpen || MobileLayout.isMobileLayout()) {
			return;
		}
		const {QuickSwitcherModal} = modalModule;
		ModalCommands.pushWithKey(
			modal(() => <QuickSwitcherModal data-flx="search.quick-switcher.quick-switcher-modal" />),
			QUICK_SWITCHER_OVERLAY_ID,
		);
	}

	hide(): void {
		if (!this.isOpen) {
			return;
		}
		this.cancelCandidateWarmup();
		this.isOpen = false;
		this.candidateSets = null;
		this.searchSources = null;
		this.defaultResults = null;
		this.query = '';
		this.prefixMode = null;
		this.results = [];
		this.selectedIndex = -1;
		if (this.memberSearchContext) {
			this.memberSearchContext.destroy();
			this.memberSearchContext = null;
		}
		if (this.memberFetchDebounceTimer) {
			clearTimeout(this.memberFetchDebounceTimer);
			this.memberFetchDebounceTimer = null;
		}
		this.isFetchingMembersInBackground = false;
		this.memberSearchResults = [];
		if (!MobileLayout.isMobileLayout()) {
			ModalCommands.popWithKey(QUICK_SWITCHER_OVERLAY_ID);
		}
	}

	private invalidateCandidateSets(): void {
		this.cancelCandidateWarmup();
		this.candidateSets = null;
		this.searchSources = null;
		if (this.isOpen) {
			this.scheduleCandidateWarmup();
		}
	}

	private getCandidateSets(i18n: I18n): CandidateSets {
		this.cancelCandidateWarmup();
		this.candidateSets ??= buildCandidateSets(i18n);
		return this.candidateSets;
	}

	private getSearchContext(i18n: I18n): QuickSwitcherSearchContext {
		const sets = this.getCandidateSets(i18n);
		this.searchSources ??= createForwardSearchCandidates(i18n).get();
		return {confusables: this.confusables, sets, sources: this.searchSources};
	}

	private scheduleCandidateWarmup(): void {
		if (this.candidateSets || this.candidateWarmupCancel) {
			return;
		}
		this.candidateWarmupCancel = this.scheduleDeferredWork(() => {
			this.candidateWarmupCancel = null;
			if (!this.isOpen || this.candidateSets) {
				return;
			}
			try {
				this.candidateSets = buildCandidateSets(this.getI18n());
			} catch (error) {
				if (error instanceof Error && error.message === QUICK_SWITCHER_I18N_MISSING_ERROR) {
					throw error;
				}
				this.logger.error('Quick switcher failed to warm search candidates', error);
			}
		}, 80);
	}

	private cancelCandidateWarmup(): void {
		this.candidateWarmupCancel?.();
		this.candidateWarmupCancel = null;
	}

	private scheduleDeferredWork(callback: () => void, fallbackDelayMs: number): () => void {
		if (typeof window !== 'undefined' && window.requestIdleCallback && window.cancelIdleCallback) {
			const handle = window.requestIdleCallback(callback, {timeout: Math.max(fallbackDelayMs, 250)});
			return () => window.cancelIdleCallback(handle);
		}
		const handle = setTimeout(callback, fallbackDelayMs);
		return () => clearTimeout(handle);
	}

	search(query: string): void {
		if (!this.isOpen && query.length === 0) {
			return;
		}
		this.defaultResults = null;
		const {prefixMode, results, selectedIndex} = this.computeResultsForQuery(query);
		this.query = query;
		this.prefixMode = prefixMode;
		this.results = results;
		this.selectedIndex = selectedIndex;
		this.triggerMemberSearchIfNeeded(query, prefixMode);
	}

	private triggerMemberSearchIfNeeded(query: string, prefixMode: QuickSwitcherQueryMode | null): void {
		if (prefixMode !== QuickSwitcherResultTypes.USER || query.startsWith(ALL_USERS_PREFIX)) {
			if (this.memberSearchContext) {
				this.memberSearchContext.destroy();
				this.memberSearchContext = null;
			}
			if (this.memberFetchDebounceTimer) {
				clearTimeout(this.memberFetchDebounceTimer);
				this.memberFetchDebounceTimer = null;
			}
			this.isFetchingMembersInBackground = false;
			this.memberSearchResults = [];
			return;
		}
		const rawSearch = query['slice'](1).trim();
		if (rawSearch.length === 0) {
			if (this.memberSearchContext) {
				this.memberSearchContext.cancelSearch();
			}
			if (this.memberFetchDebounceTimer) {
				clearTimeout(this.memberFetchDebounceTimer);
				this.memberFetchDebounceTimer = null;
			}
			this.isFetchingMembersInBackground = false;
			this.memberSearchResults = [];
			return;
		}
		if (!this.memberSearchContext) {
			this.memberSearchContext = MemberSearch.getSearchContext((results) => {
				const guildMemberRecords: Array<GuildMember> = results
					.map((transformed: TransformedMember) => resolveTransformedMember(transformed))
					.filter((member): member is GuildMember => member !== null);
				runInAction(() => {
					this.memberSearchResults = guildMemberRecords;
					if (this.isOpen && this.prefixMode === QuickSwitcherResultTypes.USER) {
						this.recomputeIfOpen({invalidateCandidates: false});
					}
				});
			}, MEMBER_SEARCH_LIMIT);
		}
		this.memberSearchContext.beginSearch(rawSearch);
		if (this.memberFetchDebounceTimer) {
			clearTimeout(this.memberFetchDebounceTimer);
		}
		const selectedGuildId = SelectedGuild.selectedGuildId;
		const guildId = selectedGuildId != null && Guilds.getGuild(selectedGuildId) != null ? selectedGuildId : null;
		const guildsToFetch = guildId != null && !GuildMembers.isGuildFullyLoaded(guildId) ? [guildId] : [];
		if (guildsToFetch.length === 0) {
			this.memberFetchDebounceTimer = null;
			this.isFetchingMembersInBackground = false;
			return;
		}
		this.memberFetchDebounceTimer = setTimeout(() => {
			this.isFetchingMembersInBackground = true;
			void MemberSearch.fetchMembersInBackground(rawSearch, guildsToFetch, guildId ?? undefined).finally(() => {
				runInAction(() => {
					this.isFetchingMembersInBackground = false;
				});
			});
			this.memberFetchDebounceTimer = null;
		}, 300);
	}

	select(selectedIndex: number): void {
		if (!this.isOpen) {
			return;
		}
		if (selectedIndex < 0) {
			this.selectedIndex = -1;
			return;
		}
		if (selectedIndex >= this.results.length) {
			return;
		}
		const result = this.results[selectedIndex];
		if (result.type === QuickSwitcherResultTypes.HEADER) {
			this.selectedIndex = -1;
			return;
		}
		this.selectedIndex = selectedIndex;
	}

	recomputeIfOpen(options: {force?: boolean; invalidateCandidates?: boolean} = {}): void {
		if (!this.isOpen) {
			return;
		}
		if (options.invalidateCandidates ?? true) {
			this.invalidateCandidateSets();
		}
		const previous = this.results[this.selectedIndex];
		const {prefixMode, results, selectedIndex} = this.computeResultsForQuery(this.query);
		if (!options.force && this.prefixMode === prefixMode && hasSameResultIdentity(this.results, results)) {
			return;
		}
		this.prefixMode = prefixMode;
		this.results = results;
		this.selectedIndex = resolveRecomputedSelectedIndex(previous, results, selectedIndex);
	}

	private getDefaultResults(i18n: I18n): Array<QuickSwitcherResult> {
		if (this.defaultResults == null || this.defaultResults.length === 0) {
			this.defaultResults = generateDefaultResults(i18n);
		}
		return this.defaultResults;
	}

	private computeResultsForQuery(query: string): ComputeResultsForQueryResult {
		const i18n = this.getI18n();
		const channelPath = parseChannelUrl(query);
		const linkResults: Array<QuickSwitcherResult> = [];
		if (channelPath) {
			const linkResult: LinkResult = {
				type: QuickSwitcherResultTypes.LINK,
				id: 'link-jump',
				title: i18n._(GO_TO_MESSAGE_DESCRIPTOR),
				subtitle: query,
				path: channelPath,
			};
			linkResults.push(linkResult);
		}
		const {mode, search, variant} = parsePrefix(query);
		let results: Array<QuickSwitcherResult>;
		if (mode != null) {
			results = generateQueryModeResults(
				mode,
				variant,
				search,
				this.getSearchContext(i18n),
				i18n,
				this.memberSearchResults,
			);
		} else if (search.length === 0) {
			results = this.getDefaultResults(i18n);
		} else {
			results = [...linkResults, ...generateGeneralResults(search, this.getSearchContext(i18n), i18n)];
		}
		return {
			prefixMode: mode,
			results,
			selectedIndex: getFirstSelectableIndex(results),
		};
	}
}

export default new QuickSwitcher();
