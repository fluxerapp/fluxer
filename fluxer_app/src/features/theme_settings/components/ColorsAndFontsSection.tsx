// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {CANCEL_DESCRIPTOR, CLEAR_SEARCH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import Theme from '@app/features/theme/state/Theme';
import ThemeEditTarget from '@app/features/theme/state/ThemeEditTarget';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import {EditNextToAppButton} from '@app/features/theme_settings/components/EditNextToAppButton';
import {TokenRow} from '@app/features/theme_settings/components/TokenRow';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import {getThemeBaseKind, getThemeDefaultVariables} from '@app/features/theme_settings/utils/ThemeDefaultVariables';
import {TRY_A_DIFFERENT_NAME_OR_CLEAR_THE_SEARCH_DESCRIPTOR} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {
	getTokenGroupLabelDescriptor,
	getTokenVariableDefinition,
	humanizeVariableName,
	TOKEN_GROUPS,
} from '@app/features/theme_settings/utils/TokenGroups';
import {Accordion} from '@app/features/ui/accordion/Accordion';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {
	extractThemeVariableOverrides,
	removeThemeVariableOverrides,
	updateCssForVariable,
} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {COLORS_AND_FONTS_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useState} from 'react';

const COLORS_AND_FONTS_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Change single colors and fonts of the base theme. Your changes are saved in the quick CSS of the theme you're editing.",
	comment:
		'Description of the "Colors and fonts" section in Settings > Themes. Quick CSS is custom CSS the user writes.',
});
const CHANGED_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# changed} other {# changed}}',
	comment:
		'Shown next to the "Colors and fonts" section title in Settings > Themes. How many colors and fonts the user changed.',
});
const CLEAR_ALL_CHANGES_DESCRIPTOR = msg({
	message: 'Clear all changes',
	comment: 'Button in the "Colors and fonts" section in Settings > Themes that removes every color and font change.',
});
const CLEAR_ALL_CHANGES_TITLE_DESCRIPTOR = msg({
	message: 'Clear all color and font changes?',
	comment: 'Confirmation title before clearing every color and font change in Settings > Themes.',
});
const CLEAR_ALL_CHANGES_CONFIRM_DESCRIPTOR = msg({
	message: 'This removes every color, font and size change. Saved themes and the rest of your quick CSS stay.',
	comment:
		'Confirmation text before clearing every color and font change in Settings > Themes. Quick CSS is custom CSS the user writes.',
});
const SEARCH_COLORS_AND_FONTS_DESCRIPTOR = msg({
	message: 'Search colors and fonts…',
	comment: 'Placeholder for the search field in the "Colors and fonts" section in Settings > Themes.',
});
const NOTHING_MATCHES_DESCRIPTOR = msg({
	message: 'Nothing matches "{searchQuery}".',
	comment: 'Empty search result in the "Colors and fonts" section in Settings > Themes.',
});

function matchesSearch(variableName: string, query: string): boolean {
	const definition = getTokenVariableDefinition(variableName);
	return [
		variableName,
		humanizeVariableName(variableName),
		definition?.groupLabel,
		definition?.kind,
		definition?.source,
	].some((value) => value?.toLowerCase().includes(query) ?? false);
}

export const ColorsAndFontsSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const customThemeCss = ThemeProfile.customThemeCssFor(ThemeEditTarget.target) ?? '';
	const overrides = useMemo(() => extractThemeVariableOverrides(customThemeCss), [customThemeCss]);
	const overrideCount = Object.keys(overrides).length;
	const defaultValues = getThemeDefaultVariables(getThemeBaseKind(Theme.themeFor(ThemeEditTarget.target)));
	const searchQuery = ThemeSettingsState.tokenSearch.trim().toLowerCase();
	const searching = searchQuery.length > 0;
	const [searchCollapsed, setSearchCollapsed] = useState<{query: string; groupIds: ReadonlySet<string>}>({
		query: searchQuery,
		groupIds: new Set(),
	});
	const collapsedWhileSearching = searchCollapsed.query === searchQuery ? searchCollapsed.groupIds : new Set<string>();
	const setGroupExpanded = (groupId: string, expanded: boolean) => {
		if (!searching) {
			ThemeSettingsState.setGroupExpanded(groupId, expanded);
			return;
		}
		const next = new Set(collapsedWhileSearching);
		if (expanded) {
			next.delete(groupId);
		} else {
			next.add(groupId);
		}
		setSearchCollapsed({query: searchQuery, groupIds: next});
	};
	const groups = useMemo(
		() =>
			TOKEN_GROUPS.map((group) => ({
				group,
				variables: searching
					? group.variables.filter((variable) => matchesSearch(variable, searchQuery))
					: group.variables,
			})).filter((entry) => entry.variables.length > 0),
		[searching, searchQuery],
	);
	const writeCss = (css: string) => {
		ThemeSettingsCommands.setEditedCustomThemeCss(css.trim().length === 0 ? null : css);
	};
	const handleClearAll = () => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(CLEAR_ALL_CHANGES_TITLE_DESCRIPTOR)}
					description={i18n._(CLEAR_ALL_CHANGES_CONFIRM_DESCRIPTOR)}
					primaryText={i18n._(CLEAR_ALL_CHANGES_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={() =>
						writeCss(removeThemeVariableOverrides(ThemeProfile.customThemeCssFor(ThemeEditTarget.target) ?? ''))
					}
					data-flx="theme-settings.colors-and-fonts-section.handle-clear-all.confirm-modal"
				/>
			)),
		);
	};
	return (
		<SettingsSection
			id="colors-and-fonts"
			title={i18n._(COLORS_AND_FONTS_SECTION_DESCRIPTOR)}
			description={i18n._(COLORS_AND_FONTS_DESCRIPTION_DESCRIPTOR)}
			actions={
				<>
					{overrideCount > 0 ? (
						<>
							<span className={styles.changedCount} data-flx="theme-settings.colors-and-fonts-section.changed-count">
								{i18n._(CHANGED_COUNT_DESCRIPTOR, {count: overrideCount})}
							</span>
							<Button
								variant="secondary"
								small
								onClick={handleClearAll}
								data-flx="theme-settings.colors-and-fonts-section.button.clear-all"
							>
								{i18n._(CLEAR_ALL_CHANGES_DESCRIPTOR)}
							</Button>
						</>
					) : null}
					<EditNextToAppButton tab="colors" data-flx="theme-settings.colors-and-fonts-section.edit-next-to-app" />
				</>
			}
			data-flx="theme-settings.colors-and-fonts-section.colors-and-fonts"
		>
			<Input
				className={styles.searchInput}
				value={ThemeSettingsState.tokenSearch}
				onChange={(event) => ThemeSettingsState.setTokenSearch(event.target.value)}
				placeholder={i18n._(SEARCH_COLORS_AND_FONTS_DESCRIPTOR)}
				aria-label={i18n._(SEARCH_COLORS_AND_FONTS_DESCRIPTOR)}
				leftIcon={
					<MagnifyingGlassIcon
						size={remFromPx(16)}
						weight="bold"
						data-flx="theme-settings.colors-and-fonts-section.magnifying-glass-icon"
					/>
				}
				data-flx="theme-settings.colors-and-fonts-section.search-input.set-token-search"
			/>
			{groups.length === 0 ? (
				<div className={styles.empty} data-flx="theme-settings.colors-and-fonts-section.empty">
					<p className={styles.emptyTitle} data-flx="theme-settings.colors-and-fonts-section.empty-title">
						{i18n._(NOTHING_MATCHES_DESCRIPTOR, {searchQuery: ThemeSettingsState.tokenSearch.trim()})}
					</p>
					<p className={styles.note} data-flx="theme-settings.colors-and-fonts-section.note">
						{i18n._(TRY_A_DIFFERENT_NAME_OR_CLEAR_THE_SEARCH_DESCRIPTOR)}
					</p>
					<div data-flx="theme-settings.colors-and-fonts-section.clear-search">
						<Button
							variant="secondary"
							small
							onClick={() => ThemeSettingsState.setTokenSearch('')}
							data-flx="theme-settings.colors-and-fonts-section.button.clear-search"
						>
							{i18n._(CLEAR_SEARCH_DESCRIPTOR)}
						</Button>
					</div>
				</div>
			) : (
				<div className={styles.list} data-flx="theme-settings.colors-and-fonts-section.list">
					{groups.map(({group, variables}) => {
						const labelDescriptor = getTokenGroupLabelDescriptor(group.id);
						return (
							<Accordion
								key={group.id}
								id={`theme-token-group-${group.id}`}
								title={
									<>
										{labelDescriptor == null ? group.fallbackLabel : i18n._(labelDescriptor)}
										<span className={styles.groupCount} data-flx="theme-settings.colors-and-fonts-section.group-count">
											{variables.length}
										</span>
									</>
								}
								expanded={
									searching ? !collapsedWhileSearching.has(group.id) : ThemeSettingsState.isGroupExpanded(group.id)
								}
								onExpandedChange={(expanded) => setGroupExpanded(group.id, expanded)}
								compact
								data-flx="theme-settings.colors-and-fonts-section.accordion"
							>
								{variables.map((variableName) => (
									<TokenRow
										key={variableName}
										variableName={variableName}
										label={humanizeVariableName(variableName)}
										kind={getTokenVariableDefinition(variableName)?.kind ?? 'other'}
										currentValue={overrides[variableName] ?? ''}
										defaultValue={defaultValues[variableName] ?? ''}
										onChange={(value) => writeCss(updateCssForVariable(customThemeCss, variableName, value))}
										data-flx="theme-settings.colors-and-fonts-section.token-row.write-css"
									/>
								))}
							</Accordion>
						);
					})}
				</div>
			)}
		</SettingsSection>
	);
});
