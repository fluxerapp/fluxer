// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import styles from '@app/features/theme_settings/components/QuickCssReference.module.css';
import {readOnScreenFlxValues, SEMANTIC_ATTRIBUTES} from '@app/features/theme_settings/editor/AppSelectors';
import EditorPreferences from '@app/features/theme_settings/editor/EditorPreferences';
import {isColorVariable, readDisplayVariableValue} from '@app/features/theme_settings/editor/ThemeVariableInfo';
import {getTokenGroupLabelDescriptor, TOKEN_GROUPS} from '@app/features/theme_settings/utils/TokenGroups';
import {MoreOptionsVerticalIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {copyText} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CopyIcon, MagnifyingGlassIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useState} from 'react';

const COPY_DESCRIPTOR = msg({
	message: 'Copy',
	comment: 'Menu item on a CSS variable in the "Reference" tab of the quick CSS editor. Copies its name.',
});
const VARIABLES_DESCRIPTOR = msg({
	message: 'Variables',
	comment: 'Heading in the "Reference" tab of the quick CSS editor, above the list of CSS variables.',
});
const SELECTORS_DESCRIPTOR = msg({
	message: 'Selectors',
	comment: 'Heading in the "Reference" tab of the quick CSS editor, above the list of CSS selectors.',
});
const SEARCH_VARIABLES_DESCRIPTOR = msg({
	message: 'Search variables',
	comment: 'Placeholder of the search field in the "Reference" tab of the quick CSS editor.',
});
const SELECT_VARIABLE_HINT_DESCRIPTOR = msg({
	message: 'Select a variable to add it to your CSS.',
	comment: 'Shown above the list of CSS variables in the "Reference" tab of the quick CSS editor.',
});
const STABLE_ATTRIBUTES_DESCRIPTOR = msg({
	message: "These attributes don't change between Fluxer updates.",
	comment: 'Shown above the list of CSS attributes in the "Reference" tab of the quick CSS editor.',
});
const ON_SCREEN_DESCRIPTOR = msg({
	message: 'On screen now',
	comment:
		'Heading in the "Reference" tab of the quick CSS editor, above the [data-flx] values of what the app shows right now.',
});
const ON_SCREEN_HINT_DESCRIPTOR = msg({
	message: 'Select one to add a rule for it to your CSS.',
	comment: 'Shown above the [data-flx] values in the "Reference" tab of the quick CSS editor.',
});
const ADD_AN_OVERRIDE_DESCRIPTOR = msg({
	message: 'Add an override',
	comment: 'Menu item on a CSS variable in the "Reference" tab of the quick CSS editor. Adds a rule that changes it.',
});
const VARIABLE_OPTIONS_DESCRIPTOR = msg({
	message: 'More options for {name}',
	comment: 'Accessible label of the menu button on a CSS variable in the "Reference" tab of the quick CSS editor.',
});
const COPIED_DESCRIPTOR = msg({
	message: 'Copied {text}.',
	comment: 'Toast after copying a CSS variable name in the "Reference" tab of the quick CSS editor.',
});
const NO_VARIABLES_MATCH_DESCRIPTOR = msg({
	message: 'No variables match "{query}".',
	comment: 'Shown in the "Reference" tab of the quick CSS editor when the search finds nothing.',
});

function indent(): string {
	return EditorPreferences.indentWithTabs ? '\t' : ' '.repeat(EditorPreferences.tabSize);
}

const VariableRow: React.FC<{name: string}> = ({name}) => {
	const {i18n} = useLingui();
	const value = readDisplayVariableValue(name);
	const openMenu = (event: React.MouseEvent<HTMLButtonElement>) => {
		ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => {
			const select = (action: () => void) => () => {
				onClose();
				action();
			};
			return (
				<MenuGroup data-flx="theme-settings.quick-css-reference.variable-row.menu-group">
					<MenuItem
						icon={
							<CopyIcon size={remFromPx(16)} data-flx="theme-settings.quick-css-reference.variable-row.copy-icon" />
						}
						onClick={select(() => {
							void copyText(name).then(() => ToastCommands.success(i18n._(COPIED_DESCRIPTOR, {text: name})));
						})}
						data-flx="theme-settings.quick-css-reference.variable-row.copy"
					>
						{i18n._(COPY_DESCRIPTOR)}
					</MenuItem>
					<MenuItem
						icon={
							<PlusIcon size={remFromPx(16)} data-flx="theme-settings.quick-css-reference.variable-row.plus-icon" />
						}
						onClick={select(() =>
							ThemeSettingsCommands.insertIntoQuickCssEditor(`:root {\n${indent()}${name}: ${value};\n}\n`),
						)}
						data-flx="theme-settings.quick-css-reference.variable-row.add-override"
					>
						{i18n._(ADD_AN_OVERRIDE_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			);
		});
	};
	return (
		<li className={styles.row} data-flx="theme-settings.quick-css-reference.variable-row.row">
			<button
				type="button"
				className={styles.rowButton}
				onClick={() => ThemeSettingsCommands.insertIntoQuickCssEditor(`var(${name})`)}
				data-flx="theme-settings.quick-css-reference.variable-row.insert"
			>
				{isColorVariable(name) && value ? (
					<span
						className={styles.swatch}
						style={{background: value}}
						aria-hidden
						data-flx="theme-settings.quick-css-reference.variable-row.swatch"
					/>
				) : null}
				<span className={styles.name} data-flx="theme-settings.quick-css-reference.variable-row.name">
					{name}
				</span>
				<span className={styles.value} title={value} data-flx="theme-settings.quick-css-reference.variable-row.value">
					{value}
				</span>
			</button>
			<button
				type="button"
				className={styles.menuButton}
				aria-label={i18n._(VARIABLE_OPTIONS_DESCRIPTOR, {name})}
				onClick={openMenu}
				data-flx="theme-settings.quick-css-reference.variable-row.menu-button"
			>
				<MoreOptionsVerticalIcon
					size={16}
					data-flx="theme-settings.quick-css-reference.variable-row.more-options-icon"
				/>
			</button>
		</li>
	);
};

export const QuickCssReference: React.FC = observer(() => {
	const {i18n} = useLingui();
	const [search, setSearch] = useState('');
	const query = search.trim().toLowerCase();
	const groups = useMemo(
		() =>
			TOKEN_GROUPS.map((group) => {
				const descriptor = getTokenGroupLabelDescriptor(group.id);
				return {
					id: group.id,
					label: descriptor ? i18n._(descriptor) : group.fallbackLabel,
					variables: query ? group.variables.filter((name) => name.toLowerCase().includes(query)) : group.variables,
				};
			}).filter((group) => group.variables.length > 0),
		[query, i18n.locale],
	);
	const [onScreen] = useState(() => readOnScreenFlxValues());
	return (
		<div className={styles.root} data-flx="theme-settings.quick-css-reference.root">
			<section className={styles.section} data-flx="theme-settings.quick-css-reference.variables">
				<h2 className={styles.heading} data-flx="theme-settings.quick-css-reference.variables-heading">
					{i18n._(VARIABLES_DESCRIPTOR)}
				</h2>
				<p className={styles.hint} data-flx="theme-settings.quick-css-reference.variables-hint">
					{i18n._(SELECT_VARIABLE_HINT_DESCRIPTOR)}
				</p>
				<Input
					value={search}
					onChange={(event) => setSearch(event.target.value)}
					placeholder={i18n._(SEARCH_VARIABLES_DESCRIPTOR)}
					aria-label={i18n._(SEARCH_VARIABLES_DESCRIPTOR)}
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="theme-settings.quick-css-reference.magnifying-glass-icon"
						/>
					}
					data-flx="theme-settings.quick-css-reference.search"
				/>
				{groups.length === 0 ? (
					<p className={styles.hint} data-flx="theme-settings.quick-css-reference.no-match">
						{i18n._(NO_VARIABLES_MATCH_DESCRIPTOR, {query: search.trim()})}
					</p>
				) : null}
				{groups.map((group) => (
					<div key={group.id} className={styles.group} data-flx="theme-settings.quick-css-reference.group">
						<h3 className={styles.groupHeading} data-flx="theme-settings.quick-css-reference.group-heading">
							{group.label}
						</h3>
						<ul className={styles.list} data-flx="theme-settings.quick-css-reference.group-list">
							{group.variables.map((name) => (
								<VariableRow key={name} name={name} data-flx="theme-settings.quick-css-reference.variable-row" />
							))}
						</ul>
					</div>
				))}
			</section>
			<section className={styles.section} data-flx="theme-settings.quick-css-reference.selectors">
				<h2 className={styles.heading} data-flx="theme-settings.quick-css-reference.selectors-heading">
					{i18n._(SELECTORS_DESCRIPTOR)}
				</h2>
				<p className={styles.hint} data-flx="theme-settings.quick-css-reference.selectors-hint">
					{i18n._(STABLE_ATTRIBUTES_DESCRIPTOR)}
				</p>
				<ul className={styles.list} data-flx="theme-settings.quick-css-reference.attribute-list">
					{SEMANTIC_ATTRIBUTES.map((attribute) => (
						<li key={attribute.name} className={styles.row} data-flx="theme-settings.quick-css-reference.attribute-row">
							<button
								type="button"
								className={styles.rowButton}
								onClick={() => ThemeSettingsCommands.insertIntoQuickCssEditor(`[${attribute.name}]`)}
								data-flx="theme-settings.quick-css-reference.attribute-insert"
							>
								<span className={styles.name} data-flx="theme-settings.quick-css-reference.attribute-name">
									{attribute.name}
								</span>
								<span
									className={styles.description}
									data-flx="theme-settings.quick-css-reference.attribute-description"
								>
									{i18n._(attribute.description)}
								</span>
							</button>
						</li>
					))}
				</ul>
				<h3 className={styles.groupHeading} data-flx="theme-settings.quick-css-reference.on-screen-heading">
					{i18n._(ON_SCREEN_DESCRIPTOR)}
				</h3>
				<p className={styles.hint} data-flx="theme-settings.quick-css-reference.on-screen-hint">
					{i18n._(ON_SCREEN_HINT_DESCRIPTOR)}
				</p>
				<ul className={styles.chips} data-flx="theme-settings.quick-css-reference.on-screen-list">
					{onScreen.map((value) => (
						<li key={value} data-flx="theme-settings.quick-css-reference.on-screen-item">
							<button
								type="button"
								className={styles.code}
								onClick={() =>
									ThemeSettingsCommands.insertIntoQuickCssEditor(`[data-flx="${value}"] {\n${indent()}\n}\n`)
								}
								data-flx="theme-settings.quick-css-reference.on-screen-insert"
							>
								{value}
							</button>
						</li>
					))}
				</ul>
			</section>
		</div>
	);
});
