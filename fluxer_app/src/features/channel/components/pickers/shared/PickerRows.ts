// SPDX-License-Identifier: AGPL-3.0-or-later

import GuildList from '@app/features/guild/state/GuildList';
import Guilds from '@app/features/guild/state/Guilds';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
export const EMPTY_PICKER_MAP: ReadonlyMap<string, unknown> = new Map();
export interface PickerHeaderRow {
	type: 'header';
	category: string;
	name: string;
	guildId?: string;
	index: number;
}
interface PickerSection<T> {
	category: string;
	name: string;
	items: ReadonlyArray<T>;
	guildId?: string;
}
export function buildPickerRows<T, R>({
	searchTerm,
	rendered,
	favorites,
	frequent,
	byGuild,
	extraSections = [],
	perRow,
	isCollapsed,
	createRow,
	favoritesLabel,
	frequentLabel,
}: {
	searchTerm: string;
	rendered: ReadonlyArray<T>;
	favorites: ReadonlyArray<T>;
	frequent: ReadonlyArray<T>;
	byGuild: ReadonlyMap<string, ReadonlyArray<T>>;
	extraSections?: ReadonlyArray<PickerSection<T>>;
	perRow: number;
	isCollapsed: (category: string) => boolean;
	createRow: (items: Array<T>, index: number, guildId?: string) => R;
	favoritesLabel: string;
	frequentLabel: string;
}): Array<PickerHeaderRow | R> {
	const rows: Array<PickerHeaderRow | R> = [];
	let index = 0;
	const addItems = (items: ReadonlyArray<T>, guildId?: string) => {
		for (let i = 0; i < items.length; i += perRow) rows.push(createRow(items.slice(i, i + perRow), index++, guildId));
	};
	if (searchTerm) {
		addItems(rendered);
		return rows;
	}
	const sections: Array<PickerSection<T>> = [];
	if (favorites.length) sections.push({category: 'favorites', name: favoritesLabel, items: favorites});
	if (frequent.length) sections.push({category: 'frequently-used', name: frequentLabel, items: frequent});
	for (const [guildId, items] of byGuild)
		sections.push({category: guildId, name: Guilds.getGuild(guildId)!.name, guildId, items});
	sections.push(...extraSections);
	for (const section of sections) {
		rows.push({
			type: 'header',
			category: section.category,
			name: section.name,
			guildId: section.guildId,
			index: index++,
		});
		if (!isCollapsed(section.category)) addItems(section.items, section.guildId);
	}
	return rows;
}
export function groupByGuildInGuildListOrder<T extends {guildId?: string}>(
	items: ReadonlyArray<T>,
): Map<string, Array<T>> {
	const byGuild = new Map<string, Array<T>>();
	for (const item of items) {
		if (!item.guildId) continue;
		const group = byGuild.get(item.guildId);
		if (group) group.push(item);
		else byGuild.set(item.guildId, [item]);
	}
	const guildIds = GuildList.guilds.map((guild) => guild.id);
	const selectedGuildId = SelectedGuild.selectedGuildId;
	if (selectedGuildId) {
		const index = guildIds.indexOf(selectedGuildId);
		if (index > 0) {
			guildIds.splice(index, 1);
			guildIds.unshift(selectedGuildId);
		}
	}
	const sorted = new Map<string, Array<T>>();
	for (const id of guildIds) {
		const group = byGuild.get(id);
		if (group) sorted.set(id, group);
	}
	return sorted;
}
