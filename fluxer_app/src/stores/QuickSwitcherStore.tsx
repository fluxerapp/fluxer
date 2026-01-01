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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {matchSorter, rankings} from 'match-sorter';
import {action, makeAutoObservable, reaction, runInAction} from 'mobx';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {
	ChannelTypes,
	FAVORITES_GUILD_ID,
	type QuickSwitcherResultType,
	QuickSwitcherResultTypes,
	RelationshipTypes,
	ThemeTypes,
} from '~/Constants';
import {
	getSettingsSubtabs,
	getSettingsTabs,
	type SettingsSubtab,
	type SettingsTab,
} from '~/components/modals/utils/settingsConstants';
import {QuickSwitcherModal} from '~/components/quick-switcher/QuickSwitcherModal';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {UserRecord} from '~/records/UserRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import ChannelStore from '~/stores/ChannelStore';
import DeveloperModeStore from '~/stores/DeveloperModeStore';
import FavoritesStore from '~/stores/FavoritesStore';
import FeatureFlagStore from '~/stores/FeatureFlagStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import MemberSearchStore, {type SearchContext, type TransformedMember} from '~/stores/MemberSearchStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import NavigationStore from '~/stores/NavigationStore';
import ReadStateStore from '~/stores/ReadStateStore';
import RelationshipStore from '~/stores/RelationshipStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import SelectedGuildStore from '~/stores/SelectedGuildStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import UserStore from '~/stores/UserStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {parseChannelUrl} from '~/utils/DeepLinkUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

const MAX_GENERAL_RESULTS = 5;
const MAX_QUERY_MODE_RESULTS = 20;
const MAX_RECENT_RESULTS = 8;
const UNREAD_SORT_WEIGHT_BOOST = 7 * 24 * 60 * 60 * 1000;
const QUICK_SWITCHER_MODAL_KEY = 'quick-switcher';
const MEMBER_SEARCH_LIMIT = 25;

type QuickSwitcherQueryMode =
	| typeof QuickSwitcherResultTypes.USER
	| typeof QuickSwitcherResultTypes.TEXT_CHANNEL
	| typeof QuickSwitcherResultTypes.VOICE_CHANNEL
	| typeof QuickSwitcherResultTypes.GUILD
	| typeof QuickSwitcherResultTypes.VIRTUAL_GUILD
	| typeof QuickSwitcherResultTypes.SETTINGS
	| typeof QuickSwitcherResultTypes.QUICK_ACTION
	| typeof QuickSwitcherResultTypes.LINK;

export interface HeaderResult {
	type: typeof QuickSwitcherResultTypes.HEADER;
	id: string;
	title: string;
}

export interface UserResult {
	type: typeof QuickSwitcherResultTypes.USER;
	id: string;
	title: string;
	subtitle?: string;
	user: UserRecord;
	dmChannelId: string | null;
	viewContext?: string;
}

export interface GroupDMResult {
	type: typeof QuickSwitcherResultTypes.GROUP_DM;
	id: string;
	title: string;
	subtitle?: string;
	channel: ChannelRecord;
	viewContext?: string;
}

export interface TextChannelResult {
	type: typeof QuickSwitcherResultTypes.TEXT_CHANNEL;
	id: string;
	title: string;
	subtitle?: string;
	channel: ChannelRecord;
	guild: GuildRecord | null;
	viewContext?: string;
}

export interface VoiceChannelResult {
	type: typeof QuickSwitcherResultTypes.VOICE_CHANNEL;
	id: string;
	title: string;
	subtitle?: string;
	channel: ChannelRecord;
	guild: GuildRecord | null;
	viewContext?: string;
}

export interface GuildResult {
	type: typeof QuickSwitcherResultTypes.GUILD;
	id: string;
	title: string;
	subtitle?: string;
	guild: GuildRecord;
}

export interface VirtualGuildResult {
	type: typeof QuickSwitcherResultTypes.VIRTUAL_GUILD;
	id: string;
	title: string;
	subtitle?: string;
	virtualGuildType: 'favorites' | 'home';
}

export interface SettingsResult {
	type: typeof QuickSwitcherResultTypes.SETTINGS;
	id: string;
	title: string;
	subtitle?: string;
	settingsTab: SettingsTab;
	settingsSubtab?: SettingsSubtab;
}

export interface QuickActionResult {
	type: typeof QuickSwitcherResultTypes.QUICK_ACTION;
	id: string;
	title: string;
	subtitle?: string;
	action: () => void;
}

export interface LinkResult {
	type: typeof QuickSwitcherResultTypes.LINK;
	id: string;
	title: string;
	subtitle?: string;
	path: string;
}

export type QuickSwitcherResult =
	| HeaderResult
	| UserResult
	| GroupDMResult
	| TextChannelResult
	| VoiceChannelResult
	| GuildResult
	| VirtualGuildResult
	| SettingsResult
	| QuickActionResult
	| LinkResult;

export type QuickSwitcherExecutableResult = Exclude<QuickSwitcherResult, HeaderResult>;

interface CandidateBase<T extends QuickSwitcherResultType> {
	type: T;
	id: string;
	title: string;
	subtitle?: string;
	searchValues: Array<string>;
	sortWeight: number;
}

export interface UserCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.USER> {
	user: UserRecord;
	dmChannelId: string | null;
}

export interface GroupDMCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.GROUP_DM> {
	channel: ChannelRecord;
}

export interface TextChannelCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.TEXT_CHANNEL> {
	channel: ChannelRecord;
	guild: GuildRecord | null;
}

export interface VoiceChannelCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.VOICE_CHANNEL> {
	channel: ChannelRecord;
	guild: GuildRecord | null;
}

export interface GuildCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.GUILD> {
	guild: GuildRecord;
}

export interface VirtualGuildCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.VIRTUAL_GUILD> {
	virtualGuildType: 'favorites' | 'home';
}

export interface SettingsCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.SETTINGS> {
	settingsTab: SettingsTab;
	settingsSubtab?: SettingsSubtab;
}

export interface QuickActionCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.QUICK_ACTION> {
	action: () => void;
}

export type Candidate =
	| UserCandidate
	| GroupDMCandidate
	| TextChannelCandidate
	| VoiceChannelCandidate
	| GuildCandidate
	| VirtualGuildCandidate
	| SettingsCandidate
	| QuickActionCandidate;

export interface CandidateSets {
	users: Array<UserCandidate>;
	userByChannelId: Map<string, UserCandidate>;
	groupDMs: Array<GroupDMCandidate>;
	groupDMByChannelId: Map<string, GroupDMCandidate>;
	textChannels: Array<TextChannelCandidate>;
	voiceChannels: Array<VoiceChannelCandidate>;
	guilds: Array<GuildCandidate>;
	virtualGuilds: Array<VirtualGuildCandidate>;
	settings: Array<SettingsCandidate>;
	quickActions: Array<QuickActionCandidate>;
	channelById: Map<string, TextChannelCandidate | VoiceChannelCandidate>;
}

class QuickSwitcherStore {
	isOpen = false;
	query = '';
	queryMode: QuickSwitcherQueryMode | null = null;
	results: Array<QuickSwitcherResult> = [];
	selectedIndex = -1;
	private memberSearchContext: SearchContext | null = null;
	private memberFetchDebounceTimer: NodeJS.Timeout | null = null;
	private memberSearchResults: Array<GuildMemberRecord> = [];
	private i18n: I18n | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});

		reaction(
			() => SelectedChannelStore.recentChannelVisits,
			() => {
				if (this.isOpen) {
					this.recomputeIfOpen();
				}
			},
		);

		reaction(
			() => [NavigationStore.guildId, NavigationStore.channelId],
			() => {
				if (this.isOpen) {
					this.recomputeIfOpen();
				}
			},
		);
	}

	setI18n(i18n: I18n): void {
		this.i18n = i18n;
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

	@action
	show(): void {
		if (this.isOpen) return;

		this.isOpen = true;
		this.query = '';
		this.queryMode = null;

		try {
			const {results, selectedIndex} = this.computeResultsForQuery('');
			this.results = results;
			this.selectedIndex = selectedIndex;
		} catch (error) {
			console.error('Quick switcher failed to precompute results', error);
			this.results = [];
			this.selectedIndex = -1;
		}

		if (!MobileLayoutStore.isMobileLayout()) {
			void this.pushModal();
		}
	}

	private pushModal(): void {
		ModalActionCreators.pushWithKey(
			modal(() => <QuickSwitcherModal />),
			QUICK_SWITCHER_MODAL_KEY,
		);
	}

	@action
	hide(): void {
		if (!this.isOpen) {
			return;
		}

		this.isOpen = false;
		this.query = '';
		this.queryMode = null;
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
		this.memberSearchResults = [];

		if (!MobileLayoutStore.isMobileLayout()) {
			ModalActionCreators.popWithKey(QUICK_SWITCHER_MODAL_KEY);
		}
	}

	@action
	search(query: string): void {
		if (!this.isOpen && query.length === 0) {
			return;
		}

		const {queryMode, results, selectedIndex} = this.computeResultsForQuery(query);
		this.query = query;
		this.queryMode = queryMode;
		this.results = results;
		this.selectedIndex = selectedIndex;

		this.triggerMemberSearchIfNeeded(query, queryMode);
	}

	private triggerMemberSearchIfNeeded(query: string, queryMode: QuickSwitcherQueryMode | null): void {
		if (queryMode !== QuickSwitcherResultTypes.USER) {
			if (this.memberSearchContext) {
				this.memberSearchContext.destroy();
				this.memberSearchContext = null;
			}
			if (this.memberFetchDebounceTimer) {
				clearTimeout(this.memberFetchDebounceTimer);
				this.memberFetchDebounceTimer = null;
			}
			this.memberSearchResults = [];
			return;
		}

		const rawSearch = query.slice(1).trim();
		if (rawSearch.length === 0) {
			if (this.memberSearchContext) {
				this.memberSearchContext.clearQuery();
			}
			if (this.memberFetchDebounceTimer) {
				clearTimeout(this.memberFetchDebounceTimer);
				this.memberFetchDebounceTimer = null;
			}
			this.memberSearchResults = [];
			return;
		}

		if (!this.memberSearchContext) {
			this.memberSearchContext = MemberSearchStore.getSearchContext((results) => {
				const guildMemberRecords: Array<GuildMemberRecord> = results
					.map((transformed) => this.resolveTransformedMember(transformed))
					.filter((member): member is GuildMemberRecord => member !== null);

				runInAction(() => {
					this.memberSearchResults = guildMemberRecords;
					if (this.isOpen && this.queryMode === QuickSwitcherResultTypes.USER) {
						this.recomputeIfOpen();
					}
				});
			}, MEMBER_SEARCH_LIMIT);
		}

		this.memberSearchContext.setQuery(rawSearch);

		if (this.memberFetchDebounceTimer) {
			clearTimeout(this.memberFetchDebounceTimer);
		}

		const currentChannelId = SelectedChannelStore.currentChannelId;
		const currentChannel = currentChannelId ? ChannelStore.getChannel(currentChannelId) : null;
		const guildId = currentChannel?.guildId ?? null;

		const allGuilds = GuildStore.getGuilds();
		const guildsToFetch = allGuilds
			.filter((guild) => !GuildMemberStore.isGuildFullyLoaded(guild.id))
			.map((guild) => guild.id);

		if (guildsToFetch.length === 0) {
			this.memberFetchDebounceTimer = null;
			return;
		}

		this.memberFetchDebounceTimer = setTimeout(() => {
			void MemberSearchStore.fetchMembersInBackground(rawSearch, guildsToFetch, guildId ?? undefined);
			this.memberFetchDebounceTimer = null;
		}, 300);
	}

	@action
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

	recomputeIfOpen(): void {
		if (!this.isOpen || !this.i18n) {
			return;
		}

		const {queryMode, results, selectedIndex} = this.computeResultsForQuery(this.query);
		this.queryMode = queryMode;
		this.results = results;
		this.selectedIndex = selectedIndex;
	}

	private computeResultsForQuery(query: string) {
		const sets = this.buildCandidateSets();

		const channelPath = parseChannelUrl(query);
		if (channelPath) {
			if (!this.i18n) {
				return {
					queryMode: null,
					results: [],
					selectedIndex: -1,
				};
			}
			const linkResult: LinkResult = {
				type: QuickSwitcherResultTypes.LINK,
				id: 'link-jump',
				title: this.i18n._(msg`Go to message`),
				subtitle: query,
				path: channelPath,
			};
			return {
				queryMode: null,
				results: [linkResult],
				selectedIndex: 0,
			};
		}

		if (query.trim().length === 0) {
			const results = this.generateDefaultResults(sets);
			return {
				queryMode: null as QuickSwitcherQueryMode | null,
				results,
				selectedIndex: this.getFirstSelectableIndex(results),
			};
		}

		const queryMode = this.getQueryMode(query);
		const rawSearch = queryMode ? query.slice(1) : query;
		const trimmedSearch = rawSearch.trim();

		let results: Array<QuickSwitcherResult>;
		if (queryMode) {
			results = this.generateQueryModeResults(queryMode, trimmedSearch, sets);
		} else if (trimmedSearch.length === 0) {
			results = this.generateDefaultResults(sets);
		} else {
			results = this.generateGeneralResults(trimmedSearch, sets);
		}

		return {
			queryMode,
			results,
			selectedIndex: this.getFirstSelectableIndex(results),
		};
	}

	private getQueryMode(query: string): QuickSwitcherQueryMode | null {
		switch (query.charAt(0)) {
			case '@':
				return QuickSwitcherResultTypes.USER;
			case '#':
				return QuickSwitcherResultTypes.TEXT_CHANNEL;
			case '!':
				return QuickSwitcherResultTypes.VOICE_CHANNEL;
			case '*':
				return QuickSwitcherResultTypes.GUILD;
			case '>':
				return QuickSwitcherResultTypes.QUICK_ACTION;
			default:
				return null;
		}
	}

	private buildCandidateSets(): CandidateSets {
		if (!this.i18n) {
			return {
				users: [],
				userByChannelId: new Map(),
				groupDMs: [],
				groupDMByChannelId: new Map(),
				textChannels: [],
				voiceChannels: [],
				guilds: [],
				virtualGuilds: [],
				settings: [],
				quickActions: [],
				channelById: new Map(),
			};
		}

		const guilds = GuildStore.getGuilds();
		const guildMap = new Map<string, GuildRecord>(guilds.map((guild) => [guild.id, guild]));

		const userCandidates = new Map<string, UserCandidate>();
		const userByChannelId = new Map<string, UserCandidate>();
		const groupDMCandidates: Array<GroupDMCandidate> = [];
		const groupDMByChannelId = new Map<string, GroupDMCandidate>();
		const textChannelCandidates: Array<TextChannelCandidate> = [];
		const voiceChannelCandidates: Array<VoiceChannelCandidate> = [];
		const channelById = new Map<string, TextChannelCandidate | VoiceChannelCandidate>();

		const currentUserId = UserStore.getCurrentUser()?.id ?? null;

		for (const channel of ChannelStore.allChannels) {
			switch (channel.type) {
				case ChannelTypes.DM:
				case ChannelTypes.DM_PERSONAL_NOTES: {
					const recipientId =
						channel.recipientIds.find((recipientId) => recipientId !== currentUserId) ?? channel.recipientIds.at(0);
					if (!recipientId) break;
					const user = UserStore.getUser(recipientId);
					if (!user) break;

					const title = ChannelUtils.getDMDisplayName(channel);
					const subtitle = user.tag;
					const searchValues = [title, subtitle, user.username, user.id].filter(Boolean);
					const baseWeight = this.getChannelRecency(channel);
					const sortWeight = this.getChannelSortWeight(channel.id, baseWeight);

					const existing = userCandidates.get(user.id);
					const candidate: UserCandidate = {
						type: QuickSwitcherResultTypes.USER,
						id: user.id,
						title,
						subtitle,
						user,
						dmChannelId: channel.id,
						searchValues,
						sortWeight,
					};

					if (!existing || existing.sortWeight < candidate.sortWeight) {
						userCandidates.set(user.id, candidate);
					} else if (existing.dmChannelId == null) {
						userCandidates.set(user.id, {
							...existing,
							dmChannelId: channel.id,
							sortWeight: Math.max(existing.sortWeight, sortWeight),
						});
					}

					const resolvedCandidate = userCandidates.get(user.id);
					if (resolvedCandidate) {
						userByChannelId.set(channel.id, resolvedCandidate);
					}
					break;
				}
				case ChannelTypes.GROUP_DM: {
					const title = ChannelUtils.getDMDisplayName(channel);
					const participantNames = channel.recipientIds
						.map((recipientId) => {
							const user = UserStore.getUser(recipientId);
							return user ? NicknameUtils.getNickname(user) : null;
						})
						.filter(Boolean) as Array<string>;
					const subtitle = participantNames.length > 0 ? participantNames.join(', ') : this.i18n._(msg`Group message`);
					const searchValues = [title, ...participantNames];

					const baseWeight = this.getChannelRecency(channel);
					const sortWeight = this.getChannelSortWeight(channel.id, baseWeight);

					const candidate: GroupDMCandidate = {
						type: QuickSwitcherResultTypes.GROUP_DM,
						id: channel.id,
						title,
						subtitle,
						channel,
						searchValues,
						sortWeight,
					};

					groupDMCandidates.push(candidate);
					groupDMByChannelId.set(channel.id, candidate);
					break;
				}
				case ChannelTypes.GUILD_TEXT: {
					if (!channel.guildId) break;
					const guild = guildMap.get(channel.guildId) ?? null;
					const title = channel.name ? channel.name : this.i18n._(msg`Unknown channel`);
					const subtitle = guild?.name;
					const searchValues = [
						channel.name ?? '',
						channel.topic ?? '',
						guild?.name ?? '',
						channel.parentId ?? '',
					].filter(Boolean);

					const baseWeight = this.getChannelRecency(channel);
					const sortWeight = this.getChannelSortWeight(channel.id, baseWeight);

					const candidate: TextChannelCandidate = {
						type: QuickSwitcherResultTypes.TEXT_CHANNEL,
						id: channel.id,
						title,
						subtitle,
						channel,
						guild,
						searchValues,
						sortWeight,
					};

					textChannelCandidates.push(candidate);
					channelById.set(channel.id, candidate);
					break;
				}
				case ChannelTypes.GUILD_VOICE: {
					if (!channel.guildId) break;
					const guild = guildMap.get(channel.guildId) ?? null;
					const title = channel.name ?? this.i18n._(msg`Voice channel`);
					const subtitle = guild?.name;
					const searchValues = [channel.name ?? '', guild?.name ?? ''].filter(Boolean);

					const baseWeight = this.getChannelRecency(channel);
					const sortWeight = this.getChannelSortWeight(channel.id, baseWeight);

					const candidate: VoiceChannelCandidate = {
						type: QuickSwitcherResultTypes.VOICE_CHANNEL,
						id: channel.id,
						title,
						subtitle,
						channel,
						guild,
						searchValues,
						sortWeight,
					};

					voiceChannelCandidates.push(candidate);
					channelById.set(channel.id, candidate);
					break;
				}
				default:
					break;
			}
		}

		for (const relationship of RelationshipStore.getRelationships()) {
			if (relationship.type !== RelationshipTypes.FRIEND) {
				continue;
			}

			const user = relationship.user;
			if (!userCandidates.has(user.id)) {
				const title = NicknameUtils.getNickname(user);
				const subtitle = user.tag;
				const searchValues = [title, subtitle, user.username, user.id].filter(Boolean);

				userCandidates.set(user.id, {
					type: QuickSwitcherResultTypes.USER,
					id: user.id,
					title,
					subtitle,
					user,
					dmChannelId: null,
					searchValues,
					sortWeight: relationship.since.getTime(),
				});
			}
		}

		for (const user of UserStore.getUsers()) {
			if (user.id === currentUserId) continue;
			if (!userCandidates.has(user.id)) {
				const title = NicknameUtils.getNickname(user);
				const subtitle = user.tag;
				const searchValues = [title, subtitle, user.username, user.id].filter(Boolean);

				userCandidates.set(user.id, {
					type: QuickSwitcherResultTypes.USER,
					id: user.id,
					title,
					subtitle,
					user,
					dmChannelId: null,
					searchValues,
					sortWeight: SnowflakeUtils.extractTimestamp(user.id),
				});
			}
		}

		const selectedGuildId = SelectedGuildStore.selectedGuildId;
		if (selectedGuildId) {
			const guildMembers = GuildMemberStore.getMembers(selectedGuildId);
			for (const member of guildMembers) {
				if (member.user.id === currentUserId) continue;
				if (!userCandidates.has(member.user.id)) {
					const title = member.nick ?? NicknameUtils.getNickname(member.user);
					const subtitle = member.user.tag;
					const searchValues = [title, subtitle, member.user.username, member.user.id, member.nick].filter(
						Boolean,
					) as Array<string>;

					userCandidates.set(member.user.id, {
						type: QuickSwitcherResultTypes.USER,
						id: member.user.id,
						title,
						subtitle,
						user: member.user,
						dmChannelId: null,
						searchValues,
						sortWeight: member.joinedAt ? new Date(member.joinedAt).getTime() : 0,
					});
				}
			}
		}

		const guildCandidates: Array<GuildCandidate> = guilds.map((guild) => ({
			type: QuickSwitcherResultTypes.GUILD,
			id: guild.id,
			title: guild.name,
			subtitle: undefined,
			guild,
			searchValues: [guild.name, guild.vanityURLCode ?? '', guild.id].filter(Boolean),
			sortWeight: guild.joinedAt ? new Date(guild.joinedAt).getTime() : 0,
		}));

		const virtualGuildCandidates: Array<VirtualGuildCandidate> = [];

		virtualGuildCandidates.push({
			type: QuickSwitcherResultTypes.VIRTUAL_GUILD,
			id: 'home',
			title: this.i18n._(msg`Home`),
			subtitle: this.i18n._(msg`Direct Messages`),
			virtualGuildType: 'home',
			searchValues: ['Home', 'DM', 'DMs', 'Direct Messages', 'Messages'],
			sortWeight: Date.now(),
		});

		if (FavoritesStore.hasAnyFavorites) {
			virtualGuildCandidates.push({
				type: QuickSwitcherResultTypes.VIRTUAL_GUILD,
				id: 'favorites',
				title: this.i18n._(msg`Favorites`),
				subtitle: undefined,
				virtualGuildType: 'favorites',
				searchValues: ['Favorites', 'Fav', 'Starred', FAVORITES_GUILD_ID],
				sortWeight: Date.now(),
			});
		}

		const settingsCandidates: Array<SettingsCandidate> = [];

		const hasExpressionPackAccess = FeatureFlagStore.isExpressionPacksEnabled(selectedGuildId ?? undefined);

		const accessibleTabs = getSettingsTabs((msg) => this.i18n!._(msg)).filter((tab) => {
			if (!DeveloperModeStore.isDeveloper && tab.category === 'staff_only') {
				return false;
			}
			if (!hasExpressionPackAccess && tab.type === 'expression_packs') {
				return false;
			}
			return true;
		});

		for (const tab of accessibleTabs) {
			settingsCandidates.push({
				type: QuickSwitcherResultTypes.SETTINGS,
				id: tab.type,
				title: tab.label,
				subtitle: undefined,
				settingsTab: tab,
				settingsSubtab: undefined,
				searchValues: [tab.label, 'settings', 'preferences', tab.type],
				sortWeight: 0,
			});
		}

		for (const subtab of getSettingsSubtabs((msg) => this.i18n!._(msg))) {
			const parentTab = accessibleTabs.find((t) => t.type === subtab.parentTab);
			if (!parentTab) continue;

			settingsCandidates.push({
				type: QuickSwitcherResultTypes.SETTINGS,
				id: `${subtab.parentTab}_${subtab.type}`,
				title: subtab.label,
				subtitle: parentTab.label,
				settingsTab: parentTab,
				settingsSubtab: subtab,
				searchValues: [subtab.label, parentTab.label, 'settings', subtab.type],
				sortWeight: 0,
			});
		}

		const quickActionCandidates: Array<QuickActionCandidate> = [];

		quickActionCandidates.push({
			type: QuickSwitcherResultTypes.QUICK_ACTION,
			id: 'toggle_theme',
			title: this.i18n._(msg`Toggle Theme`),
			subtitle: this.i18n._(msg`Switch between Light and Dark mode`),
			action: () => {
				const currentTheme = UserSettingsStore.getTheme();
				const newTheme = currentTheme === ThemeTypes.DARK ? ThemeTypes.LIGHT : ThemeTypes.DARK;
				UserSettingsStore.saveSettings({theme: newTheme});
			},
			searchValues: ['theme', 'light', 'dark', 'mode', 'switch', 'toggle'],
			sortWeight: 0,
		});

		quickActionCandidates.push({
			type: QuickSwitcherResultTypes.QUICK_ACTION,
			id: 'toggle_compact_mode',
			title: this.i18n._(msg`Toggle Compact Mode`),
			subtitle: UserSettingsStore.getMessageDisplayCompact()
				? this.i18n._(msg`Disable Compact Mode`)
				: this.i18n._(msg`Enable Compact Mode`),
			action: () => {
				UserSettingsStore.saveSettings({
					messageDisplayCompact: !UserSettingsStore.getMessageDisplayCompact(),
				});
			},
			searchValues: ['compact', 'mode', 'display', 'message', 'toggle'],
			sortWeight: 0,
		});

		quickActionCandidates.push({
			type: QuickSwitcherResultTypes.QUICK_ACTION,
			id: 'toggle_reduced_motion',
			title: this.i18n._(msg`Toggle Reduced Motion`),
			subtitle: AccessibilityStore.useReducedMotion
				? this.i18n._(msg`Disable Reduced Motion`)
				: this.i18n._(msg`Enable Reduced Motion`),
			action: () => {
				AccessibilityStore.updateSettings({
					reducedMotionOverride: !AccessibilityStore.useReducedMotion,
					syncReducedMotionWithSystem: false,
				});
			},
			searchValues: ['reduced', 'motion', 'animation', 'toggle'],
			sortWeight: 0,
		});

		return {
			users: Array.from(userCandidates.values()),
			userByChannelId,
			groupDMs: groupDMCandidates,
			groupDMByChannelId,
			textChannels: textChannelCandidates,
			voiceChannels: voiceChannelCandidates,
			guilds: guildCandidates,
			virtualGuilds: virtualGuildCandidates,
			settings: settingsCandidates,
			quickActions: quickActionCandidates,
			channelById,
		};
	}

	private getExcludedChannelIds(): Set<string> {
		const excluded = new Set<string>();

		const currentChannelId = this.getCurrentChannelId();
		if (!currentChannelId) return excluded;

		excluded.add(currentChannelId);

		const currentChannel = ChannelStore.getChannel(currentChannelId);
		if (currentChannel?.parentId) {
			excluded.add(currentChannel.parentId);
		}

		return excluded;
	}

	private getCurrentChannelId(): string | null {
		return NavigationStore.channelId ?? SelectedChannelStore.currentChannelId;
	}

	private generateDefaultResults(sets: CandidateSets): Array<QuickSwitcherResult> {
		if (!this.i18n) {
			return [];
		}

		const recentVisits = SelectedChannelStore.recentChannelVisits;
		const excludedIds = this.getExcludedChannelIds();

		const recentResults: Array<QuickSwitcherExecutableResult> = [];

		for (const visit of recentVisits) {
			if (excludedIds.has(visit.channelId)) continue;
			const channel = ChannelStore.getChannel(visit.channelId);
			if (!channel) continue;
			const result = this.createResultFromChannel(channel, sets, visit.guildId);
			if (result) {
				recentResults.push(result);
			}
		}

		const recentSliced = recentResults.slice(0, MAX_RECENT_RESULTS);

		if (recentSliced.length === 0) {
			return [];
		}

		return [this.createHeaderResult('recent', this.i18n._(msg`Recently visited`)), ...recentSliced];
	}

	private generateQueryModeResults(
		queryMode: QuickSwitcherQueryMode,
		search: string,
		sets: CandidateSets,
	): Array<QuickSwitcherResult> {
		let candidates: Array<Candidate>;
		switch (queryMode) {
			case QuickSwitcherResultTypes.USER:
				candidates = this.buildUserCandidatesWithMemberSearch(sets.users);
				break;
			case QuickSwitcherResultTypes.TEXT_CHANNEL:
				candidates = sets.textChannels;
				break;
			case QuickSwitcherResultTypes.VOICE_CHANNEL:
				candidates = sets.voiceChannels;
				break;
			case QuickSwitcherResultTypes.GUILD:
				candidates = [...sets.guilds, ...sets.virtualGuilds];
				break;
			case QuickSwitcherResultTypes.VIRTUAL_GUILD:
				candidates = sets.virtualGuilds;
				break;
			case QuickSwitcherResultTypes.SETTINGS:
				candidates = sets.settings;
				break;
			case QuickSwitcherResultTypes.QUICK_ACTION:
				candidates = sets.quickActions;
				break;
			default:
				candidates = [];
		}

		if (
			search.length === 0 &&
			(queryMode === QuickSwitcherResultTypes.TEXT_CHANNEL || queryMode === QuickSwitcherResultTypes.VOICE_CHANNEL)
		) {
			const excludedIds = this.getExcludedChannelIds();
			candidates = candidates.filter((c) => !excludedIds.has(c.id));
		}

		const matches = this.matchCandidates(candidates, search, MAX_QUERY_MODE_RESULTS);
		if (matches.length === 0) {
			return [];
		}

		return [
			this.createHeaderResult(`query-${queryMode}`, this.getHeaderTitle(queryMode)),
			...matches.map((c) => this.candidateToResult(c)),
		];
	}

	private generateGeneralResults(search: string, sets: CandidateSets): Array<QuickSwitcherResult> {
		const sections: Array<{type: QuickSwitcherResultType; headerId: string; candidates: Array<Candidate>}> = [
			{type: QuickSwitcherResultTypes.USER, headerId: 'people', candidates: sets.users},
			{type: QuickSwitcherResultTypes.GROUP_DM, headerId: 'group-dm', candidates: sets.groupDMs},
			{type: QuickSwitcherResultTypes.TEXT_CHANNEL, headerId: 'text-channels', candidates: sets.textChannels},
			{type: QuickSwitcherResultTypes.VOICE_CHANNEL, headerId: 'voice-channels', candidates: sets.voiceChannels},
			{type: QuickSwitcherResultTypes.GUILD, headerId: 'guilds', candidates: [...sets.guilds, ...sets.virtualGuilds]},
			{type: QuickSwitcherResultTypes.SETTINGS, headerId: 'settings', candidates: sets.settings},
			{type: QuickSwitcherResultTypes.QUICK_ACTION, headerId: 'quick-actions', candidates: sets.quickActions},
		];

		const results: Array<QuickSwitcherResult> = [];

		for (const section of sections) {
			const matches = this.matchCandidates(section.candidates, search, MAX_GENERAL_RESULTS);
			if (matches.length === 0) continue;

			results.push(this.createHeaderResult(`section-${section.headerId}`, this.getHeaderTitle(section.type)));
			results.push(...matches.map((candidate) => this.candidateToResult(candidate)));
		}

		return results;
	}

	private buildUserCandidatesWithMemberSearch(baseCandidates: Array<UserCandidate>): Array<UserCandidate> {
		if (this.memberSearchResults.length === 0) {
			return baseCandidates;
		}

		const candidateMap = new Map<string, UserCandidate>();
		for (const candidate of baseCandidates) {
			candidateMap.set(candidate.user.id, candidate);
		}

		const currentUserId = UserStore.getCurrentUser()?.id ?? null;
		for (const member of this.memberSearchResults) {
			const userId = member.user.id;
			if (currentUserId && userId === currentUserId) {
				continue;
			}
			if (candidateMap.has(userId)) {
				continue;
			}

			candidateMap.set(userId, this.createUserCandidateFromMember(member));
		}

		return Array.from(candidateMap.values());
	}

	private createUserCandidateFromMember(member: GuildMemberRecord): UserCandidate {
		const title = member.nick ?? NicknameUtils.getNickname(member.user);
		const subtitle = member.user.tag;
		const searchValues = [title, subtitle, member.user.username, member.user.id, member.nick].filter(
			Boolean,
		) as Array<string>;

		return {
			type: QuickSwitcherResultTypes.USER,
			id: member.user.id,
			title,
			subtitle,
			user: member.user,
			dmChannelId: null,
			searchValues,
			sortWeight: member.joinedAt ? member.joinedAt.getTime() : 0,
		};
	}

	private resolveTransformedMember(member: TransformedMember): GuildMemberRecord | null {
		const guildIds = member.guildIds ?? [];
		for (const guildId of guildIds) {
			const record = GuildMemberStore.getMember(guildId, member.id);
			if (record) {
				return record;
			}
		}

		for (const guild of GuildStore.getGuilds()) {
			const record = GuildMemberStore.getMember(guild.id, member.id);
			if (record) {
				return record;
			}
		}

		return null;
	}

	private matchCandidates<T extends Candidate>(candidates: Array<T>, search: string, limit: number): Array<T> {
		if (candidates.length === 0) {
			return [];
		}

		if (search.length === 0) {
			return this.sortCandidatesByWeight(candidates).slice(0, limit);
		}

		const results = matchSorter(candidates, search, {
			keys: [
				'title',
				{minRanking: rankings.CONTAINS, key: 'subtitle'},
				{minRanking: rankings.CONTAINS, key: (item) => item.searchValues},
			],
		});

		return results.slice(0, limit);
	}

	private sortCandidatesByWeight<T extends Candidate>(candidates: Array<T>): Array<T> {
		return [...candidates].sort((a, b) => {
			if (b.sortWeight !== a.sortWeight) {
				return b.sortWeight - a.sortWeight;
			}
			return a.title.localeCompare(b.title);
		});
	}

	private createResultFromChannel(
		channel: ChannelRecord,
		sets: CandidateSets,
		viewContext?: string,
	): QuickSwitcherExecutableResult | null {
		switch (channel.type) {
			case ChannelTypes.DM:
			case ChannelTypes.DM_PERSONAL_NOTES: {
				const candidate = sets.userByChannelId.get(channel.id);
				return candidate ? this.candidateToResult(candidate, viewContext) : null;
			}
			case ChannelTypes.GROUP_DM: {
				const candidate = sets.groupDMByChannelId.get(channel.id);
				return candidate ? this.candidateToResult(candidate, viewContext) : null;
			}
			case ChannelTypes.GUILD_TEXT: {
				const candidate = sets.channelById.get(channel.id);
				if (candidate && candidate.type === QuickSwitcherResultTypes.TEXT_CHANNEL) {
					return this.candidateToResult(candidate, viewContext);
				}
				return null;
			}
			case ChannelTypes.GUILD_VOICE: {
				const candidate = sets.channelById.get(channel.id);
				if (candidate && candidate.type === QuickSwitcherResultTypes.VOICE_CHANNEL) {
					return this.candidateToResult(candidate, viewContext);
				}
				return null;
			}
			default:
				return null;
		}
	}

	private candidateToResult(candidate: Candidate, viewContext?: string): QuickSwitcherExecutableResult {
		switch (candidate.type) {
			case QuickSwitcherResultTypes.USER:
				return {
					type: QuickSwitcherResultTypes.USER,
					id: candidate.id,
					title: candidate.title,
					subtitle: candidate.subtitle,
					user: candidate.user,
					dmChannelId: candidate.dmChannelId,
					viewContext,
				};
			case QuickSwitcherResultTypes.GROUP_DM:
				return {
					type: QuickSwitcherResultTypes.GROUP_DM,
					id: candidate.id,
					title: candidate.title,
					subtitle: candidate.subtitle,
					channel: candidate.channel,
					viewContext,
				};
			case QuickSwitcherResultTypes.TEXT_CHANNEL: {
				const isFavoritesContext = viewContext === FAVORITES_GUILD_ID;
				return {
					type: QuickSwitcherResultTypes.TEXT_CHANNEL,
					id: candidate.id,
					title: candidate.title,
					subtitle: isFavoritesContext ? (this.i18n?._(msg`Favorites`) ?? 'Favorites') : candidate.subtitle,
					channel: candidate.channel,
					guild: candidate.guild,
					viewContext,
				};
			}
			case QuickSwitcherResultTypes.VOICE_CHANNEL: {
				const isFavoritesContext = viewContext === FAVORITES_GUILD_ID;
				return {
					type: QuickSwitcherResultTypes.VOICE_CHANNEL,
					id: candidate.id,
					title: candidate.title,
					subtitle: isFavoritesContext ? (this.i18n?._(msg`Favorites`) ?? 'Favorites') : candidate.subtitle,
					channel: candidate.channel,
					guild: candidate.guild,
					viewContext,
				};
			}
			case QuickSwitcherResultTypes.GUILD:
				return {
					type: QuickSwitcherResultTypes.GUILD,
					id: candidate.id,
					title: candidate.title,
					subtitle: candidate.subtitle,
					guild: candidate.guild,
				};
			case QuickSwitcherResultTypes.VIRTUAL_GUILD:
				return {
					type: QuickSwitcherResultTypes.VIRTUAL_GUILD,
					id: candidate.id,
					title: candidate.title,
					subtitle: candidate.subtitle,
					virtualGuildType: candidate.virtualGuildType,
				};
			case QuickSwitcherResultTypes.SETTINGS:
				return {
					type: QuickSwitcherResultTypes.SETTINGS,
					id: candidate.id,
					title: candidate.title,
					subtitle: candidate.subtitle,
					settingsTab: candidate.settingsTab,
					settingsSubtab: candidate.settingsSubtab,
				};
			case QuickSwitcherResultTypes.QUICK_ACTION:
				return {
					type: QuickSwitcherResultTypes.QUICK_ACTION,
					id: candidate.id,
					title: candidate.title,
					subtitle: candidate.subtitle,
					action: candidate.action,
				};
			default:
				return candidate as never;
		}
	}

	private createHeaderResult(id: string, title: string): HeaderResult {
		return {type: QuickSwitcherResultTypes.HEADER, id, title};
	}

	private getHeaderTitle(type: QuickSwitcherResultType): string {
		if (!this.i18n) {
			switch (type) {
				case QuickSwitcherResultTypes.USER:
					return 'People';
				case QuickSwitcherResultTypes.GROUP_DM:
					return 'Group messages';
				case QuickSwitcherResultTypes.TEXT_CHANNEL:
					return 'Text channels';
				case QuickSwitcherResultTypes.VOICE_CHANNEL:
					return 'Voice channels';
				case QuickSwitcherResultTypes.GUILD:
				case QuickSwitcherResultTypes.VIRTUAL_GUILD:
					return 'Communities';
				case QuickSwitcherResultTypes.SETTINGS:
					return 'Settings';
				case QuickSwitcherResultTypes.QUICK_ACTION:
					return 'Quick Actions';
				default:
					return '';
			}
		}
		switch (type) {
			case QuickSwitcherResultTypes.USER:
				return this.i18n._(msg`People`);
			case QuickSwitcherResultTypes.GROUP_DM:
				return this.i18n._(msg`Group messages`);
			case QuickSwitcherResultTypes.TEXT_CHANNEL:
				return this.i18n._(msg`Text channels`);
			case QuickSwitcherResultTypes.VOICE_CHANNEL:
				return this.i18n._(msg`Voice channels`);
			case QuickSwitcherResultTypes.GUILD:
			case QuickSwitcherResultTypes.VIRTUAL_GUILD:
				return this.i18n._(msg`Communities`);
			case QuickSwitcherResultTypes.SETTINGS:
				return this.i18n._(msg`Settings`);
			case QuickSwitcherResultTypes.QUICK_ACTION:
				return this.i18n._(msg`Quick Actions`);
			default:
				return '';
		}
	}

	private getFirstSelectableIndex(results: ReadonlyArray<QuickSwitcherResult>): number {
		for (let i = 0; i < results.length; i += 1) {
			if (results[i].type !== QuickSwitcherResultTypes.HEADER) {
				return i;
			}
		}
		return -1;
	}

	private getChannelRecency(channel: ChannelRecord): number {
		if (channel.lastMessageId) {
			return SnowflakeUtils.extractTimestamp(channel.lastMessageId);
		}
		return SnowflakeUtils.extractTimestamp(channel.id);
	}

	private getChannelSortWeight(channelId: string, baseWeight: number): number {
		const unreadCount = ReadStateStore.getUnreadCount(channelId);
		const mentionCount = ReadStateStore.getMentionCount(channelId);
		const hasUnread = unreadCount > 0 || mentionCount > 0;

		return hasUnread ? baseWeight + UNREAD_SORT_WEIGHT_BOOST : baseWeight;
	}
}

export default new QuickSwitcherStore();
