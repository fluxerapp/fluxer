// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {downloadTextFile} from '@app/features/platform/utils/DownloadFile';
import * as ThemeProfileCommands from '@app/features/theme/commands/ThemeProfileCommands';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import ThemeEditTarget from '@app/features/theme/state/ThemeEditTarget';
import ThemeLibrary, {
	getThemeLabel,
	ThemeFolderImportFailedError,
	type ThemeLibraryFolder,
	type ThemeLibraryTheme,
} from '@app/features/theme/state/ThemeLibrary';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import {EditNextToAppButton} from '@app/features/theme_settings/components/EditNextToAppButton';
import {ThemeDetailsModal} from '@app/features/theme_settings/components/ThemeDetailsModal';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import {
	getLinkedThemeFileProblem,
	isLinkedThemeReadOnly,
	LINKED_FILE_DESCRIPTOR,
} from '@app/features/theme_settings/utils/LinkedThemeFile';
import {TRY_A_DIFFERENT_NAME_OR_CLEAR_THE_SEARCH_DESCRIPTOR} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {MoreOptionsVerticalIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {createThemeExportFileName} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {YOUR_THEMES_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ArrowDownIcon,
	ArrowUpIcon,
	CopyIcon,
	DownloadSimpleIcon,
	FileCssIcon,
	FolderOpenIcon,
	LinkBreakIcon,
	LinkSimpleIcon,
	MagnifyingGlassIcon,
	PencilSimpleIcon,
	TrashIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useRef} from 'react';

const YOUR_THEMES_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Themes you import from CSS files. The ones you turn on are added on top of the base theme in the order shown.',
	comment: 'Description of the "Your themes" section in Settings > Themes.',
});
const SEARCH_THEMES_DESCRIPTOR = msg({
	message: 'Search themes…',
	comment: 'Placeholder for the search field in the "Your themes" section in Settings > Themes.',
});
const NO_THEMES_SAVED_YET_DESCRIPTOR = msg({
	message: 'No themes saved yet.',
	comment: 'Empty state in the "Your themes" section in Settings > Themes.',
});
const IMPORT_A_CSS_FILE_TO_GET_STARTED_DESCRIPTOR = msg({
	message: 'Import a .css file to get started, or write your own CSS in quick CSS.',
	comment: 'Empty state in the "Your themes" section in Settings > Themes. Quick CSS is custom CSS the user writes.',
});
const WRITE_QUICK_CSS_DESCRIPTOR = msg({
	message: 'Write quick CSS',
	comment: 'Button in the empty state of "Your themes" that opens the quick CSS editor on its CSS tab.',
});
const FOLDER_LIMIT_DESCRIPTOR = msg({
	message: 'You can watch up to {limit} folders. Stop watching one to add another.',
	comment: 'Error toast in "Your themes" when importing a folder while the most folders are already watched.',
});
const FOLDER_UNREADABLE_DESCRIPTOR = msg({
	message: "Fluxer can't read this folder.",
	comment: 'Error toast in "Your themes" when the chosen theme folder cannot be opened.',
});
const NO_THEMES_MATCH_YOUR_SEARCH_DESCRIPTOR = msg({
	message: 'No themes match your search.',
	comment: 'Empty search result in the "Your themes" section in Settings > Themes.',
});
const ENABLE_DESCRIPTOR = msg({
	message: 'Enable {themeName}',
	comment: 'Accessible label for the switch that enables a saved theme file.',
});
const OPTIONS_FOR_THEME_DESCRIPTOR = msg({
	message: 'Options for {themeName}',
	comment: 'Accessible label for the menu button on a theme row in Settings > Themes.',
});
const EDIT_DETAILS_DESCRIPTOR = msg({
	message: 'Edit details',
	comment: 'Menu item on a theme in Settings > Themes that opens a modal to change its name, author, version and tags.',
});
const EDIT_CSS_DESCRIPTOR = msg({
	message: 'Edit CSS',
	comment: 'Menu item on a theme in Settings > Themes that opens its CSS in the quick CSS editor window.',
});
const VIEW_CSS_DESCRIPTOR = msg({
	message: 'View CSS',
	comment:
		'Menu item on a theme linked to a file in Settings > Themes. Opens its CSS read-only in the quick CSS editor window.',
});
const MOVE_UP_DESCRIPTOR = msg({
	message: 'Move up',
	comment:
		'Menu item on a turned-on theme in Settings > Themes. Moves the theme up in the list so it applies earlier, and themes below it win when they change the same thing.',
});
const MOVE_DOWN_DESCRIPTOR = msg({
	message: 'Move down',
	comment:
		'Menu item on a turned-on theme in Settings > Themes. Moves the theme down in the list so it applies later and wins over themes above it.',
});
const LINK_FILE_DESCRIPTOR = msg({
	message: 'Link file',
	comment:
		'Menu item in Settings > Themes that links an existing theme to a CSS file on disk so saved changes to the file apply live.',
});
const UNLINK_DESCRIPTOR = msg({
	message: 'Unlink',
	comment:
		'Menu item in Settings > Themes that stops a theme following its CSS file on disk and turns it back into an editable copy.',
});
const EXPORT_DESCRIPTOR = msg({
	message: 'Export',
	comment: 'Menu item in Settings > Themes that downloads a theme as a CSS file.',
});
const DUPLICATE_DESCRIPTOR = msg({
	message: 'Duplicate',
	comment: 'Menu item in Settings > Themes that makes a copy of a theme.',
});
const DELETE_THEME_DESCRIPTOR = msg({
	message: 'Delete theme',
	comment: 'Menu item and confirm button in Settings > Themes that deletes a theme from this device.',
});
const DELETE_THEME_CONFIRM_DESCRIPTOR = msg({
	message: 'Delete theme?',
	comment: 'Confirmation title before deleting a theme in Settings > Themes.',
});
const THIS_REMOVES_FROM_THIS_DEVICE_DESCRIPTOR = msg({
	message: 'This removes "{themeName}" from this device.',
	comment: 'Confirmation text before deleting a saved theme in Settings > Themes.',
});
const IMPORTED_THEME_FILES_DESCRIPTOR = msg({
	message: 'Imported {length, plural, one {# theme file} other {# theme files}}.',
	comment: 'Toast after importing one or more CSS files into the theme library.',
});
const LINKED_THEME_FILES_DESCRIPTOR = msg({
	message:
		'Linked {length, plural, one {# theme file} other {# theme files}}. Changes you save to them apply right away.',
	comment:
		'Toast after importing CSS files or a folder on the desktop app. The themes stay linked to the files on disk and update when the file is saved.',
});

const WATCHING_FOLDER_DESCRIPTOR = msg({
	message: 'Watching {path}',
	comment:
		'Shown for a watched theme folder in Settings > Themes on the desktop app. {path} is the full path of the folder. New, changed, renamed and deleted CSS files in it update the themes.',
});
const FOLDER_MISSING_DESCRIPTOR = msg({
	message: "Fluxer can't find this folder. It will load the themes again when the folder is back.",
	comment: 'Shown for a watched theme folder in Settings > Themes when the folder was moved, deleted or disconnected.',
});
const FOLDER_NOT_ALLOWED_DESCRIPTOR = msg({
	message: "Fluxer can't open this folder. Import it again to keep watching it.",
	comment:
		'Shown for a watched theme folder in Settings > Themes when the desktop app no longer has permission to read it.',
});
const FOLDER_TRUNCATED_DESCRIPTOR = msg({
	message: 'Only the first 200 CSS files in this folder are loaded.',
	comment: 'Shown for a watched theme folder in Settings > Themes when it has more than 200 CSS files.',
});
const STOP_WATCHING_DESCRIPTOR = msg({
	message: 'Stop watching',
	context: 'theme-folder',
	comment: 'Button on a watched theme folder in Settings > Themes, and the confirm button in its confirmation.',
});
const STOP_WATCHING_CONFIRM_TITLE_DESCRIPTOR = msg({
	message: 'Stop watching this folder?',
	comment: 'Confirmation title in Settings > Themes before it stops watching a theme folder.',
});
const STOP_WATCHING_CONFIRM_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Themes from this folder will be removed from "Your themes".',
	comment:
		'Confirmation text in Settings > Themes before it stops watching a theme folder. The CSS files on disk are not deleted.',
});
const FROM_FOLDER_DESCRIPTOR = msg({
	message: 'From {folder}',
	comment:
		'Shown under a theme in Settings > Themes that comes from a watched folder. {folder} is the name of the folder.',
});

const SEARCH_THRESHOLD = 5;

function folderName(path: string): string {
	return path.split(/[\\/]/).filter(Boolean).pop() ?? path;
}

const FolderRow: React.FC<{folder: ThemeLibraryFolder}> = observer(({folder}) => {
	const {i18n} = useLingui();
	const handleStopWatching = () => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(STOP_WATCHING_CONFIRM_TITLE_DESCRIPTOR)}
					description={i18n._(STOP_WATCHING_CONFIRM_DESCRIPTION_DESCRIPTOR)}
					primaryText={i18n._(STOP_WATCHING_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={() => {
						void ThemeLibrary.stopWatchingFolder(folder.path);
					}}
					data-flx="theme-settings.your-themes-section.folder-row.confirm-modal"
				/>
			)),
		);
	};
	let status: React.ReactNode = (
		<span className={styles.rowDescription} data-flx="theme-settings.your-themes-section.folder-row.watching">
			{i18n._(WATCHING_FOLDER_DESCRIPTOR, {path: folder.path})}
		</span>
	);
	if (folder.status !== 'live') {
		status = (
			<span className={styles.rowWarning} data-flx="theme-settings.your-themes-section.folder-row.warning">
				<WarningCircleIcon
					size={remFromPx(14)}
					weight="bold"
					className={styles.rowWarningIcon}
					aria-hidden
					data-flx="theme-settings.your-themes-section.folder-row.warning-icon"
				/>
				{i18n._(folder.status === 'missing' ? FOLDER_MISSING_DESCRIPTOR : FOLDER_NOT_ALLOWED_DESCRIPTOR)}
			</span>
		);
	}
	return (
		<div className={styles.row} data-flx="theme-settings.your-themes-section.folder-row.row">
			<FolderOpenIcon
				size={remFromPx(18)}
				className={styles.rowIcon}
				aria-hidden
				data-flx="theme-settings.your-themes-section.folder-row.icon"
			/>
			<div className={styles.rowText} data-flx="theme-settings.your-themes-section.folder-row.text">
				<span className={styles.rowLabel} data-flx="theme-settings.your-themes-section.folder-row.label">
					{folderName(folder.path)}
				</span>
				{status}
				{folder.truncated ? (
					<span className={styles.rowDescription} data-flx="theme-settings.your-themes-section.folder-row.truncated">
						{i18n._(FOLDER_TRUNCATED_DESCRIPTOR)}
					</span>
				) : null}
			</div>
			<div className={styles.rowControls} data-flx="theme-settings.your-themes-section.folder-row.controls">
				<Button
					variant="secondary"
					small
					onClick={handleStopWatching}
					data-flx="theme-settings.your-themes-section.folder-row.button.stop-watching"
				>
					{i18n._(STOP_WATCHING_DESCRIPTOR)}
				</Button>
			</div>
		</div>
	);
});

function matchesSearch(theme: ThemeLibraryTheme, query: string): boolean {
	return (
		theme.name.toLowerCase().includes(query) ||
		theme.description.toLowerCase().includes(query) ||
		theme.fileName.toLowerCase().includes(query) ||
		theme.tags.some((tag) => tag.toLowerCase().includes(query))
	);
}

const ThemeRow: React.FC<{theme: ThemeLibraryTheme}> = observer(({theme}) => {
	const {i18n} = useLingui();
	const target = ThemeEditTarget.target;
	const enabledIds = ThemeLibrary.enabledIdsFor(target);
	const enabledIndex = enabledIds.indexOf(theme.id);
	const enabled = enabledIndex >= 0;
	const label = getThemeLabel(theme);
	const problem = getLinkedThemeFileProblem(i18n, theme);
	const fromFolder = ThemeLibrary.isWatchedFolderTheme(theme);
	const canLinkFiles = isDesktop() && Boolean(getElectronAPI()?.pickThemeLinkedFiles);
	const handleDelete = () => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_THEME_CONFIRM_DESCRIPTOR)}
					description={i18n._(THIS_REMOVES_FROM_THIS_DEVICE_DESCRIPTOR, {themeName: label})}
					primaryText={i18n._(DELETE_THEME_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={() => {
						void ThemeLibrary.deleteTheme(theme.id);
					}}
					data-flx="theme-settings.your-themes-section.handle-delete.confirm-modal"
				/>
			)),
		);
	};
	const openMenu = (event: React.MouseEvent<HTMLButtonElement>) => {
		ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => {
			const select = (action: () => void) => () => {
				onClose();
				action();
			};
			return (
				<>
					<MenuGroup data-flx="theme-settings.your-themes-section.open-menu.menu-group">
						<MenuItem
							icon={
								<PencilSimpleIcon
									size={remFromPx(16)}
									data-flx="theme-settings.your-themes-section.open-menu.pencil-simple-icon"
								/>
							}
							onClick={select(() =>
								ModalCommands.push(
									modal(() => (
										<ThemeDetailsModal
											themeId={theme.id}
											data-flx="theme-settings.your-themes-section.open-menu.theme-details-modal"
										/>
									)),
								),
							)}
							data-flx="theme-settings.your-themes-section.open-menu.menu-item.select"
						>
							{i18n._(EDIT_DETAILS_DESCRIPTOR)}
						</MenuItem>
						{ThemeSettingsCommands.canOpenQuickCssEditor() ? (
							<MenuItem
								icon={
									<FileCssIcon
										size={remFromPx(16)}
										data-flx="theme-settings.your-themes-section.open-menu.file-css-icon"
									/>
								}
								onClick={select(() => ThemeSettingsCommands.openQuickCssEditor(theme.id))}
								data-flx="theme-settings.your-themes-section.open-menu.menu-item.select"
							>
								{i18n._(isLinkedThemeReadOnly(theme) ? VIEW_CSS_DESCRIPTOR : EDIT_CSS_DESCRIPTOR)}
							</MenuItem>
						) : null}
						{enabled && enabledIndex > 0 ? (
							<MenuItem
								icon={
									<ArrowUpIcon
										size={remFromPx(16)}
										data-flx="theme-settings.your-themes-section.open-menu.arrow-up-icon"
									/>
								}
								onClick={select(() => {
									void ThemeProfileCommands.moveEnabledTheme(theme.id, -1, target);
								})}
								data-flx="theme-settings.your-themes-section.open-menu.menu-item.move-up"
							>
								{i18n._(MOVE_UP_DESCRIPTOR)}
							</MenuItem>
						) : null}
						{enabled && enabledIndex < enabledIds.length - 1 ? (
							<MenuItem
								icon={
									<ArrowDownIcon
										size={remFromPx(16)}
										data-flx="theme-settings.your-themes-section.open-menu.arrow-down-icon"
									/>
								}
								onClick={select(() => {
									void ThemeProfileCommands.moveEnabledTheme(theme.id, 1, target);
								})}
								data-flx="theme-settings.your-themes-section.open-menu.menu-item.move-down"
							>
								{i18n._(MOVE_DOWN_DESCRIPTOR)}
							</MenuItem>
						) : null}
						{canLinkFiles && !theme.linkedPath ? (
							<MenuItem
								icon={
									<LinkSimpleIcon
										size={remFromPx(16)}
										data-flx="theme-settings.your-themes-section.open-menu.link-simple-icon"
									/>
								}
								onClick={select(() => {
									void ThemeLibrary.linkThemeToDesktopFile(theme.id);
								})}
								data-flx="theme-settings.your-themes-section.open-menu.menu-item.select--2"
							>
								{i18n._(LINK_FILE_DESCRIPTOR)}
							</MenuItem>
						) : null}
						{theme.linkedPath && !fromFolder ? (
							<MenuItem
								icon={
									<LinkBreakIcon
										size={remFromPx(16)}
										data-flx="theme-settings.your-themes-section.open-menu.link-break-icon"
									/>
								}
								onClick={select(() => {
									void ThemeLibrary.unlinkTheme(theme.id);
								})}
								data-flx="theme-settings.your-themes-section.open-menu.menu-item.select--3"
							>
								{i18n._(UNLINK_DESCRIPTOR)}
							</MenuItem>
						) : null}
						<MenuItem
							icon={
								<DownloadSimpleIcon
									size={remFromPx(16)}
									data-flx="theme-settings.your-themes-section.open-menu.download-simple-icon"
								/>
							}
							onClick={select(() =>
								downloadTextFile(theme.css, createThemeExportFileName(theme), 'text/css;charset=utf-8'),
							)}
							data-flx="theme-settings.your-themes-section.open-menu.menu-item.select--4"
						>
							{i18n._(EXPORT_DESCRIPTOR)}
						</MenuItem>
						<MenuItem
							icon={<CopyIcon size={remFromPx(16)} data-flx="theme-settings.your-themes-section.open-menu.copy-icon" />}
							onClick={select(() => {
								void ThemeLibrary.duplicateTheme(theme.id);
							})}
							data-flx="theme-settings.your-themes-section.open-menu.menu-item.select--5"
						>
							{i18n._(DUPLICATE_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
					{fromFolder ? null : (
						<MenuGroup data-flx="theme-settings.your-themes-section.open-menu.menu-group--2">
							<MenuItem
								icon={
									<TrashIcon size={remFromPx(16)} data-flx="theme-settings.your-themes-section.open-menu.trash-icon" />
								}
								danger
								onClick={select(handleDelete)}
								data-flx="theme-settings.your-themes-section.open-menu.menu-item.select--6"
							>
								{i18n._(DELETE_THEME_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					)}
				</>
			);
		});
	};
	const rowClass = styles.row;
	const textClass = styles.rowText;
	const labelNode = (
		<span className={styles.rowLabel} data-flx="theme-settings.your-themes-section.theme-row.row-label">
			{label}
		</span>
	);
	const fileNameNode = (
		<span className={styles.rowMeta} data-flx="theme-settings.your-themes-section.theme-row.row-meta">
			{theme.fileName}
		</span>
	);
	let description: React.ReactNode = null;
	if (problem) {
		description = (
			<span className={styles.rowWarning} data-flx="theme-settings.your-themes-section.theme-row.row-warning">
				<WarningCircleIcon
					size={remFromPx(14)}
					weight="bold"
					className={styles.rowWarningIcon}
					aria-hidden
					data-flx="theme-settings.your-themes-section.theme-row.row-warning-icon"
				/>
				{problem}
			</span>
		);
	} else if (fromFolder && theme.folderPath) {
		description = (
			<span className={styles.rowDescription} data-flx="theme-settings.your-themes-section.theme-row.from-folder">
				{i18n._(FROM_FOLDER_DESCRIPTOR, {folder: folderName(theme.folderPath)})}
			</span>
		);
	} else if (theme.linkedPath) {
		description = (
			<span className={styles.rowDescription} data-flx="theme-settings.your-themes-section.theme-row.row-description">
				{i18n._(LINKED_FILE_DESCRIPTOR)}
			</span>
		);
	}
	return (
		<div className={rowClass} data-flx="theme-settings.your-themes-section.theme-row.row">
			<div className={textClass} data-flx="theme-settings.your-themes-section.theme-row.row-text">
				{labelNode}
				{fileNameNode}
				{description}
			</div>
			<div className={styles.rowControls} data-flx="theme-settings.your-themes-section.theme-row.row-controls">
				<Switch
					compact
					ariaLabel={i18n._(ENABLE_DESCRIPTOR, {themeName: label})}
					value={enabled}
					onChange={(value) => {
						void ThemeProfileCommands.setThemeEnabled(theme.id, value, target);
					}}
					data-flx="theme-settings.your-themes-section.theme-row.switch"
				/>
				<FocusRing offset={-2} data-flx="theme-settings.your-themes-section.theme-row.focus-ring">
					<button
						type="button"
						className={styles.iconButton}
						aria-label={i18n._(OPTIONS_FOR_THEME_DESCRIPTOR, {themeName: label})}
						onClick={openMenu}
						data-flx="theme-settings.your-themes-section.theme-row.icon-button.open-menu"
					>
						<MoreOptionsVerticalIcon
							size={18}
							data-flx="theme-settings.your-themes-section.theme-row.more-options-vertical-icon"
						/>
					</button>
				</FocusRing>
			</div>
		</div>
	);
});

export const YourThemesSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const cssImportRef = useRef<HTMLInputElement | null>(null);
	const themes = ThemeLibrary.themes;
	const searchQuery = ThemeSettingsState.librarySearch.trim().toLowerCase();
	const showSearch = themes.length > SEARCH_THRESHOLD;
	const searching = showSearch && searchQuery.length > 0;
	const enabledIds = ThemeLibrary.enabledIdsFor(ThemeEditTarget.target);
	const visibleThemes = useMemo(() => {
		const matching = searching ? themes.filter((theme) => matchesSearch(theme, searchQuery)) : themes;
		const enabledOrder = new Map(enabledIds.map((id, index) => [id, index]));
		const enabled = matching
			.filter((theme) => enabledOrder.has(theme.id))
			.sort((a, b) => (enabledOrder.get(a.id) ?? 0) - (enabledOrder.get(b.id) ?? 0));
		return [...enabled, ...matching.filter((theme) => !enabledOrder.has(theme.id))];
	}, [themes, searching, searchQuery, enabledIds, ThemeLibrary.revision]);
	const canLinkFiles = isDesktop() && Boolean(getElectronAPI()?.pickThemeLinkedFiles);
	const canImportFolder = isDesktop() && Boolean(getElectronAPI()?.pickThemeLocalFiles);
	const handleImportCss = (event: React.ChangeEvent<HTMLInputElement>) => {
		const files = [...(event.target.files ?? [])];
		event.target.value = '';
		if (files.length === 0) return;
		void ThemeLibrary.importCssFiles(files).then((imported) => {
			ToastCommands.success(i18n._(IMPORTED_THEME_FILES_DESCRIPTOR, {length: imported.length}));
		});
	};
	const handleImportDesktopCss = () => {
		void ThemeLibrary.importDesktopCssFiles().then((imported) => {
			if (imported.length === 0) return;
			ToastCommands.success(i18n._(LINKED_THEME_FILES_DESCRIPTOR, {length: imported.length}));
		});
	};
	const handleImportFolder = () => {
		void ThemeLibrary.importDesktopThemeDirectory()
			.then((imported) => {
				if (imported.length === 0) return;
				const descriptor = imported.some((theme) => theme.linkedPath)
					? LINKED_THEME_FILES_DESCRIPTOR
					: IMPORTED_THEME_FILES_DESCRIPTOR;
				ToastCommands.success(i18n._(descriptor, {length: imported.length}));
			})
			.catch((error: unknown) => {
				if (!(error instanceof ThemeFolderImportFailedError)) throw error;
				const {reason} = error;
				ToastCommands.error(
					reason.error === 'folder_limit'
						? i18n._(FOLDER_LIMIT_DESCRIPTOR, {limit: reason.limit})
						: i18n._(FOLDER_UNREADABLE_DESCRIPTOR),
				);
			});
	};
	return (
		<SettingsSection
			id="your-themes"
			title={i18n._(YOUR_THEMES_SECTION_DESCRIPTOR)}
			description={i18n._(YOUR_THEMES_DESCRIPTION_DESCRIPTOR)}
			actions={
				<>
					<Button
						variant="secondary"
						small
						leftIcon={<FileCssIcon size={remFromPx(16)} data-flx="theme-settings.your-themes-section.file-css-icon" />}
						onClick={canLinkFiles ? handleImportDesktopCss : () => cssImportRef.current?.click()}
						data-flx="theme-settings.your-themes-section.button.import-desktop-css"
					>
						<Trans>Import CSS</Trans>
					</Button>
					{canImportFolder ? (
						<Button
							variant="secondary"
							small
							leftIcon={
								<FolderOpenIcon size={remFromPx(16)} data-flx="theme-settings.your-themes-section.folder-open-icon" />
							}
							onClick={handleImportFolder}
							data-flx="theme-settings.your-themes-section.button.import-folder"
						>
							<Trans>Import folder</Trans>
						</Button>
					) : null}
					<EditNextToAppButton tab="themes" data-flx="theme-settings.your-themes-section.edit-next-to-app" />
					<input
						ref={cssImportRef}
						type="file"
						accept=".css,text/css,text/plain"
						multiple
						hidden
						onChange={handleImportCss}
						aria-hidden
						data-flx="theme-settings.your-themes-section.input.import-css.file"
					/>
				</>
			}
			data-flx="theme-settings.your-themes-section.your-themes"
		>
			{showSearch ? (
				<Input
					className={styles.searchInput}
					value={ThemeSettingsState.librarySearch}
					onChange={(event) => ThemeSettingsState.setLibrarySearch(event.target.value)}
					placeholder={i18n._(SEARCH_THEMES_DESCRIPTOR)}
					aria-label={i18n._(SEARCH_THEMES_DESCRIPTOR)}
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(16)}
							data-flx="theme-settings.your-themes-section.magnifying-glass-icon"
						/>
					}
					data-flx="theme-settings.your-themes-section.search-input.set-library-search"
				/>
			) : null}
			{ThemeLibrary.canWatchFolders && ThemeLibrary.folders.length > 0 ? (
				<div className={styles.list} data-flx="theme-settings.your-themes-section.folders">
					{ThemeLibrary.folders.map((folder) => (
						<FolderRow key={folder.path} folder={folder} data-flx="theme-settings.your-themes-section.folder-row" />
					))}
				</div>
			) : null}
			{visibleThemes.length === 0 ? (
				<div className={styles.empty} data-flx="theme-settings.your-themes-section.empty">
					<p className={styles.emptyTitle} data-flx="theme-settings.your-themes-section.empty-title">
						{i18n._(searching ? NO_THEMES_MATCH_YOUR_SEARCH_DESCRIPTOR : NO_THEMES_SAVED_YET_DESCRIPTOR)}
					</p>
					<p className={styles.note} data-flx="theme-settings.your-themes-section.note">
						{i18n._(
							searching
								? TRY_A_DIFFERENT_NAME_OR_CLEAR_THE_SEARCH_DESCRIPTOR
								: IMPORT_A_CSS_FILE_TO_GET_STARTED_DESCRIPTOR,
						)}
					</p>
					{searching || !ThemeSettingsCommands.canOpenQuickCssEditor() ? null : (
						<div data-flx="theme-settings.your-themes-section.empty-actions">
							<Button
								variant="secondary"
								small
								onClick={() => ThemeSettingsCommands.openQuickCssEditor()}
								data-flx="theme-settings.your-themes-section.button.write-quick-css"
							>
								{i18n._(WRITE_QUICK_CSS_DESCRIPTOR)}
							</Button>
						</div>
					)}
				</div>
			) : (
				<div className={styles.list} data-flx="theme-settings.your-themes-section.list">
					{visibleThemes.map((theme) => (
						<ThemeRow key={theme.id} theme={theme} data-flx="theme-settings.your-themes-section.theme-row" />
					))}
				</div>
			)}
		</SettingsSection>
	);
});
