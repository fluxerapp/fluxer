// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IconProps} from '@phosphor-icons/react';
import {
	ArrowsDownUpIcon,
	AtIcon,
	BrowserIcon,
	CalendarBlankIcon,
	CalendarDotsIcon,
	ClockCounterClockwiseIcon,
	FileIcon,
	GlobeIcon,
	HashIcon,
	LinkIcon,
	PaperclipIcon,
	PushPinIcon,
	RobotIcon,
	SortAscendingIcon,
	TextAaIcon,
	UserIcon,
	WarningIcon,
} from '@phosphor-icons/react';

export const SearchFilterCategory = Object.freeze({
	USER: 'user',
	CHANNEL: 'channel',
	HAS: 'has',
	DATE: 'date',
	BOOLEAN: 'boolean',
	VALUE: 'value',
} as const);

export type SearchFilterCategory = (typeof SearchFilterCategory)[keyof typeof SearchFilterCategory];

interface SearchFilterMeta {
	Icon: React.ComponentType<IconProps>;
	operator: string;
	category: SearchFilterCategory;
}

const META: Record<string, SearchFilterMeta> = {
	from: {Icon: UserIcon, operator: 'from', category: SearchFilterCategory.USER},
	mentions: {Icon: AtIcon, operator: 'mentions', category: SearchFilterCategory.USER},
	in: {Icon: HashIcon, operator: 'in', category: SearchFilterCategory.CHANNEL},
	has: {Icon: PaperclipIcon, operator: 'has', category: SearchFilterCategory.HAS},
	before: {Icon: CalendarBlankIcon, operator: 'before', category: SearchFilterCategory.DATE},
	after: {Icon: CalendarBlankIcon, operator: 'after', category: SearchFilterCategory.DATE},
	during: {Icon: CalendarDotsIcon, operator: 'during', category: SearchFilterCategory.DATE},
	on: {Icon: CalendarDotsIcon, operator: 'on', category: SearchFilterCategory.DATE},
	pinned: {Icon: PushPinIcon, operator: 'pinned', category: SearchFilterCategory.BOOLEAN},
	'author-type': {Icon: RobotIcon, operator: 'author', category: SearchFilterCategory.VALUE},
	sort: {Icon: SortAscendingIcon, operator: 'sort', category: SearchFilterCategory.VALUE},
	order: {Icon: ArrowsDownUpIcon, operator: 'order', category: SearchFilterCategory.VALUE},
	mature: {Icon: WarningIcon, operator: 'mature', category: SearchFilterCategory.BOOLEAN},
	'embed-type': {Icon: BrowserIcon, operator: 'embed', category: SearchFilterCategory.VALUE},
	'embed-provider': {Icon: GlobeIcon, operator: 'provider', category: SearchFilterCategory.VALUE},
	link: {Icon: LinkIcon, operator: 'link', category: SearchFilterCategory.VALUE},
	filename: {Icon: FileIcon, operator: 'file', category: SearchFilterCategory.VALUE},
	ext: {Icon: FileIcon, operator: 'ext', category: SearchFilterCategory.VALUE},
	last: {Icon: ClockCounterClockwiseIcon, operator: 'last', category: SearchFilterCategory.VALUE},
	beforeid: {Icon: HashIcon, operator: 'before', category: SearchFilterCategory.VALUE},
	afterid: {Icon: HashIcon, operator: 'after', category: SearchFilterCategory.VALUE},
	any: {Icon: TextAaIcon, operator: 'any', category: SearchFilterCategory.VALUE},
	scope: {Icon: GlobeIcon, operator: 'scope', category: SearchFilterCategory.VALUE},
};

const FALLBACK_META: SearchFilterMeta = {Icon: HashIcon, operator: 'filter', category: SearchFilterCategory.VALUE};

export function getSearchFilterMeta(filterKey: string): SearchFilterMeta {
	const meta = META[filterKey.replace(/^-/, '')];
	if (meta == null) {
		return FALLBACK_META;
	}
	return meta;
}
