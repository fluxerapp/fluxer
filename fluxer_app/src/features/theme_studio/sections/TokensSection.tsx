// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ShareThemeModal} from '@app/features/theme/components/modals/ShareThemeModal';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {showThemeStudioErrorModal} from '@app/features/theme_studio/utils/ThemeStudioErrorModalUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {
	extractThemeVariableOverrides,
	updateCssForVariable,
} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {ArrowCounterClockwiseIcon, ShareNetworkIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo} from 'react';
import {broadcastThemeStudioMessage} from '../state/ThemeStudioBroadcast';
import ThemeStudioState from '../state/ThemeStudioState';
import {StudioButton} from '../ui/StudioButton';
import {StudioEmptyState} from '../ui/StudioEmptyState';
import {StudioSearchInput} from '../ui/StudioSearchInput';
import {StudioSection} from '../ui/StudioSection';
import {StudioTokenColor, StudioTokenFont, StudioTokenValue} from '../ui/StudioToken';
import {
	DEFAULT_EXPANDED_GROUP_IDS,
	getTokenVariableDefinition,
	humanizeVariableName,
	TOKEN_GROUPS,
} from './TokenGroups';
import styles from './TokensSection.module.css';

const TOKEN_OVERRIDES_CLEARED_DESCRIPTOR = msg({
	message: 'Token overrides cleared.',
	comment: 'Short label in the theme studio tokens section. Keep it concise. Keep the tone plain and specific.',
});
const YOU_DON_T_HAVE_ANY_CUSTOM_THEME_OVERRIDES_DESCRIPTOR = msg({
	message: "You don't have any custom theme overrides to share yet.",
	comment: 'Description text in the theme studio tokens section. Keep the tone plain and specific.',
});
const SEARCH_TOKENS_DESCRIPTOR = msg({
	message: 'Search tokens…',
	comment:
		'Button or menu action label in the theme studio tokens section. Keep it concise. Keep the tone plain and specific.',
});
const NO_TOKENS_MATCH_DESCRIPTOR = msg({
	message: 'No tokens match "{searchQuery}".',
	comment: 'Empty search result title in Theme Studio token search.',
});
const TRY_A_DIFFERENT_VARIABLE_NAME_OR_CLEAR_THE_DESCRIPTOR = msg({
	message: 'Try a different variable name or clear the search.',
	comment: 'Accessible label in the theme studio tokens section. Keep it concise. Keep the tone plain and specific.',
});

interface TokensSectionProps {
	defaultVariableValues: Readonly<Record<string, string>>;
}

export const TokensSection: React.FC<TokensSectionProps> = observer(({defaultVariableValues}) => {
	const {i18n} = useLingui();
	const customThemeCss = Accessibility.customThemeCss ?? '';
	const overrides = useMemo(() => extractThemeVariableOverrides(customThemeCss), [customThemeCss]);
	const searchQuery = ThemeStudioState.tokenSearch.trim().toLowerCase();
	useEffect(() => {
		if (!ThemeStudioState.hasInitializedExpansion) {
			ThemeStudioState.expandAllGroups(DEFAULT_EXPANDED_GROUP_IDS);
		}
	}, []);
	const handleVariableChange = (variableName: string, nextValue: string | null) => {
		const updated =
			nextValue === null
				? updateCssForVariable(customThemeCss, variableName, null)
				: updateCssForVariable(customThemeCss, variableName, nextValue);
		AccessibilityCommands.update({customThemeCss: updated.length === 0 ? null : updated});
		broadcastThemeStudioMessage({type: 'customThemeCss', value: updated.length === 0 ? null : updated});
	};
	const overrideCount = Object.keys(overrides).length;
	const tokenGroupCount = TOKEN_GROUPS.length;
	const tokenCount = TOKEN_GROUPS.reduce((count, group) => count + group.variables.length, 0);
	const handleResetAll = () => {
		if (overrideCount === 0) return;
		AccessibilityCommands.update({customThemeCss: null});
		broadcastThemeStudioMessage({type: 'customThemeCss', value: null});
		ToastCommands.success(i18n._(TOKEN_OVERRIDES_CLEARED_DESCRIPTOR));
	};
	const handleShare = () => {
		const css = [ThemeLibrary.activeThemeCss, customThemeCss]
			.map((value) => value.trim())
			.filter(Boolean)
			.join('\n\n');
		if (!css.trim()) {
			showThemeStudioErrorModal(
				i18n,
				() => i18n._(YOU_DON_T_HAVE_ANY_CUSTOM_THEME_OVERRIDES_DESCRIPTOR),
				'theme-studio.tokens-section.share-empty-error-modal',
			);
			return;
		}
		ModalCommands.push(
			modal(() => (
				<ShareThemeModal themeCss={css} data-flx="theme-studio.tokens-section.handle-share.share-theme-modal" />
			)),
		);
	};
	const groups = useMemo(() => {
		if (searchQuery.length === 0) {
			return TOKEN_GROUPS.map((group) => ({group, vars: [...group.variables]}));
		}
		return TOKEN_GROUPS.map((group) => {
			const vars = group.variables.filter((variable) => {
				const human = humanizeVariableName(variable);
				const definition = getTokenVariableDefinition(variable);
				return [variable, human, definition?.groupLabel, definition?.source, definition?.kind].some(
					(value) => value?.toLowerCase().includes(searchQuery) ?? false,
				);
			});
			return {group, vars};
		}).filter((entry) => entry.vars.length > 0);
	}, [searchQuery]);
	const noResults = groups.length === 0;
	return (
		<div className={styles.section} data-flx="theme-studio.tokens-section.section">
			<div className={styles.toolbar} data-flx="theme-studio.tokens-section.toolbar">
				<div className={styles.searchWrap} data-flx="theme-studio.tokens-section.search-wrap">
					<StudioSearchInput
						value={ThemeStudioState.tokenSearch}
						onChange={(value) => ThemeStudioState.setTokenSearch(value)}
						placeholder={i18n._(SEARCH_TOKENS_DESCRIPTOR)}
						data-flx="theme-studio.tokens-section.studio-search-input.set-token-search"
					/>
				</div>
				<div className={styles.toolbarActions} data-flx="theme-studio.tokens-section.toolbar-actions">
					<StudioButton
						variant="secondary"
						compact
						leadingIcon={
							<ArrowCounterClockwiseIcon
								size={remFromPx(13)}
								weight="bold"
								data-flx="theme-studio.tokens-section.arrow-counter-clockwise-icon"
							/>
						}
						disabled={overrideCount === 0}
						onClick={handleResetAll}
						data-flx="theme-studio.tokens-section.studio-button.reset-all"
					>
						<Trans>Reset all</Trans>
					</StudioButton>
					<StudioButton
						variant="primary"
						compact
						leadingIcon={
							<ShareNetworkIcon
								size={remFromPx(13)}
								weight="bold"
								data-flx="theme-studio.tokens-section.share-network-icon"
							/>
						}
						onClick={handleShare}
						data-flx="theme-studio.tokens-section.studio-button.share"
					>
						<Trans>Share active theme</Trans>
					</StudioButton>
				</div>
			</div>
			<div className={styles.summary} data-flx="theme-studio.tokens-section.summary">
				<span data-flx="theme-studio.tokens-section.span">
					<Plural
						value={overrideCount}
						one="# override"
						other="# overrides"
						data-flx="theme-studio.tokens-section.plural"
					/>
				</span>
				<span data-flx="theme-studio.tokens-section.span--2">
					<Plural
						value={tokenGroupCount}
						one="# group"
						other="# groups"
						data-flx="theme-studio.tokens-section.plural--2"
					/>
				</span>
				<span data-flx="theme-studio.tokens-section.span--3">
					<Plural value={tokenCount} one="# token" other="# tokens" data-flx="theme-studio.tokens-section.plural--3" />
				</span>
			</div>
			<div className={styles.body} data-flx="theme-studio.tokens-section.body">
				{noResults ? (
					<div className={styles.searchEmpty} data-flx="theme-studio.tokens-section.search-empty">
						<StudioEmptyState
							title={i18n._(NO_TOKENS_MATCH_DESCRIPTOR, {searchQuery: ThemeStudioState.tokenSearch})}
							description={i18n._(TRY_A_DIFFERENT_VARIABLE_NAME_OR_CLEAR_THE_DESCRIPTOR)}
							actions={
								<StudioButton
									variant="secondary"
									compact
									onClick={() => ThemeStudioState.setTokenSearch('')}
									data-flx="theme-studio.tokens-section.studio-button.set-token-search"
								>
									<Trans>Clear search</Trans>
								</StudioButton>
							}
							data-flx="theme-studio.tokens-section.studio-empty-state"
						/>
					</div>
				) : (
					groups.map(({group, vars}) => {
						const isOpen = searchQuery.length > 0 || ThemeStudioState.isGroupExpanded(group.id);
						return (
							<StudioSection
								key={group.id}
								title={group.fallbackLabel}
								count={vars.length}
								open={isOpen}
								onToggle={(next) => {
									if (searchQuery.length > 0) return;
									ThemeStudioState.toggleGroup(group.id, next);
								}}
								data-flx="theme-studio.tokens-section.studio-section"
							>
								{vars.map((variableName) => {
									const definition = getTokenVariableDefinition(variableName);
									const overrideValue = overrides[variableName] ?? '';
									const defaultValue = defaultVariableValues[variableName] ?? '';
									const label = humanizeVariableName(variableName);
									const overridden = overrideValue.length > 0;
									if (definition?.kind === 'font') {
										return (
											<StudioTokenFont
												key={variableName}
												variableName={variableName}
												label={label}
												currentValue={overrideValue}
												defaultValue={defaultValue}
												overridden={overridden}
												onChange={(next) => handleVariableChange(variableName, next)}
												data-flx="theme-studio.tokens-section.studio-token-font.variable-change"
											/>
										);
									}
									if (definition?.kind !== 'color') {
										return (
											<StudioTokenValue
												key={variableName}
												variableName={variableName}
												label={label}
												kind={definition?.kind ?? 'other'}
												currentValue={overrideValue}
												defaultValue={defaultValue}
												overridden={overridden}
												onChange={(next) => handleVariableChange(variableName, next)}
												data-flx="theme-studio.tokens-section.studio-token-value.variable-change"
											/>
										);
									}
									return (
										<StudioTokenColor
											key={variableName}
											variableName={variableName}
											label={label}
											currentValue={overrideValue || defaultValue}
											defaultValue={defaultValue}
											overridden={overridden}
											onChange={(next) => handleVariableChange(variableName, next)}
											data-flx="theme-studio.tokens-section.studio-token-color.variable-change"
										/>
									);
								})}
							</StudioSection>
						);
					})
				)}
			</div>
		</div>
	);
});
