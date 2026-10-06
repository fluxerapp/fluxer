// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import type {SettingsSubtab, SettingsTab} from '@app/features/user/components/settings_utils/SettingsConstants';
import type {User} from '@app/features/user/models/User';
import type {QuickSwitcherResultType, QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import {DAYS_PER_WEEK, MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';

export const UNREAD_SORT_WEIGHT_BOOST = DAYS_PER_WEEK * MS_PER_DAY;
export const QUICK_SWITCHER_OVERLAY_ID = 'nav_quick_switcher';
export const MEMBER_SEARCH_LIMIT = 25;

export type QuickSwitcherQueryMode =
	| typeof QuickSwitcherResultTypes.USER
	| typeof QuickSwitcherResultTypes.TEXT_CHANNEL
	| typeof QuickSwitcherResultTypes.VOICE_CHANNEL
	| typeof QuickSwitcherResultTypes.GUILD
	| typeof QuickSwitcherResultTypes.VIRTUAL_GUILD
	| typeof QuickSwitcherResultTypes.SETTINGS;

export interface ComputeResultsForQueryResult {
	prefixMode: QuickSwitcherQueryMode | null;
	results: Array<QuickSwitcherResult>;
	selectedIndex: number;
}

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
	user: User;
	dmChannelId: string | null;
	viewContext?: string;
}

export interface GroupDMResult {
	type: typeof QuickSwitcherResultTypes.GROUP_DM;
	id: string;
	title: string;
	subtitle?: string;
	channel: Channel;
	viewContext?: string;
}

export interface TextChannelResult {
	type: typeof QuickSwitcherResultTypes.TEXT_CHANNEL;
	id: string;
	title: string;
	subtitle?: string;
	channel: Channel;
	guild: Guild | null;
	viewContext?: string;
}

export interface VoiceChannelResult {
	type: typeof QuickSwitcherResultTypes.VOICE_CHANNEL;
	id: string;
	title: string;
	subtitle?: string;
	channel: Channel;
	guild: Guild | null;
	viewContext?: string;
}

export interface GuildResult {
	type: typeof QuickSwitcherResultTypes.GUILD;
	id: string;
	title: string;
	subtitle?: string;
	guild: Guild;
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
	user: User;
	dmChannelId: string | null;
}

export interface GroupDMCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.GROUP_DM> {
	channel: Channel;
}

export interface TextChannelCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.TEXT_CHANNEL> {
	channel: Channel;
	guild: Guild | null;
}

export interface VoiceChannelCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.VOICE_CHANNEL> {
	channel: Channel;
	guild: Guild | null;
}

export interface GuildCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.GUILD> {
	guild: Guild;
}

export interface VirtualGuildCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.VIRTUAL_GUILD> {
	virtualGuildType: 'favorites' | 'home';
}

export interface SettingsCandidate extends CandidateBase<typeof QuickSwitcherResultTypes.SETTINGS> {
	settingsTab: SettingsTab;
	settingsSubtab?: SettingsSubtab;
}

export type Candidate =
	| UserCandidate
	| GroupDMCandidate
	| TextChannelCandidate
	| VoiceChannelCandidate
	| GuildCandidate
	| VirtualGuildCandidate
	| SettingsCandidate;

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
	channelById: Map<string, TextChannelCandidate | VoiceChannelCandidate>;
}
