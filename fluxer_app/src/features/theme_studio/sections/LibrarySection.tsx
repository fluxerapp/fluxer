// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {CANCEL_DESCRIPTOR, DESCRIPTION_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {createDefaultLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {upsertThemeCssHeader} from '@app/features/theme/utils/ThemeCssUtils';
import styles from '@app/features/theme_studio/sections/LibrarySection.module.css';
import type {QuickCssEditorProps} from '@app/features/theme_studio/sections/QuickCssEditor';
import {broadcastThemeStudioMessage} from '@app/features/theme_studio/state/ThemeStudioBroadcast';
import ThemeStudioState from '@app/features/theme_studio/state/ThemeStudioState';
import {StudioButton} from '@app/features/theme_studio/ui/StudioButton';
import {StudioEmptyState} from '@app/features/theme_studio/ui/StudioEmptyState';
import {StudioSearchInput} from '@app/features/theme_studio/ui/StudioSearchInput';
import {StudioSplit} from '@app/features/theme_studio/ui/StudioSplit';
import {showThemeStudioErrorModal} from '@app/features/theme_studio/utils/ThemeStudioErrorModalUtils';
import type {ThemeStudioBaseTheme} from '@app/features/theme_studio/utils/ThemeStudioPinnedVariables';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {
	createThemeExportFileName,
	downloadTextFile,
	parseTagInput,
	readFileText,
} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	CopyIcon,
	DownloadSimpleIcon,
	FileCssIcon,
	FilePlusIcon,
	FloppyDiskIcon,
	FolderOpenIcon,
	TrashIcon,
	UploadSimpleIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useMemo, useRef, useState} from 'react';

const THEME_SAVED_DESCRIPTOR = msg({
	message: 'Theme saved.',
	comment: 'Short label in the theme studio library section. Keep it concise.',
});
const DELETE_THEME_DESCRIPTOR = msg({
	message: 'Delete theme?',
	comment: 'Confirmation prompt in the theme studio library section. Keep the tone plain and specific.',
});
const THIS_REMOVES_FROM_THIS_DEVICE_DESCRIPTOR = msg({
	message: 'This removes "{themeName}" from this device.',
	comment: 'Confirmation text before deleting a saved local theme from Theme Studio.',
});
const DELETE_THEME_2_DESCRIPTOR = msg({
	message: 'Delete theme',
	comment:
		'Button or menu action label in the theme studio library section. Keep it concise. Keep the tone plain and specific.',
});
const IMPORTED_OTHER_DESCRIPTOR = msg({
	message: 'Imported {length, plural, one {# theme file} other {# theme files}}.',
	comment: 'Toast after importing one or more CSS files into the theme library.',
});
const IMPORTED_OTHER_2_DESCRIPTOR = msg({
	message: 'Imported {length, plural, one {# theme file} other {# theme files}}.',
	comment: 'Toast after importing one or more CSS files from a local theme folder.',
});
const IMPORTED_OTHER_AND_OTHER_DESCRIPTOR = msg({
	message:
		'Imported {themes, plural, one {# theme} other {# themes}} and {assets, plural, one {# asset} other {# assets}}.',
	comment: 'Toast after importing a Theme Studio library export file.',
});
const WE_COULDN_T_IMPORT_THAT_THEME_LIBRARY_FILE_DESCRIPTOR = msg({
	message: "We couldn't import that theme library file.",
	comment: 'Error message in the theme studio library section.',
});
const SEARCH_THEMES_DESCRIPTOR = msg({
	message: 'Search themes…',
	comment: 'Button or menu action label in the theme studio library section. Keep it concise.',
});
const NO_THEMES_MATCH_YOUR_SEARCH_DESCRIPTOR = msg({
	message: 'No themes match your search.',
	comment: 'Empty-state text in the theme studio library section.',
});
const NO_THEMES_SAVED_YET_DESCRIPTOR = msg({
	message: 'No themes saved yet.',
	comment: 'Empty-state text in the theme studio library section.',
});
const TRY_A_DIFFERENT_NAME_OR_CLEAR_THE_SEARCH_DESCRIPTOR = msg({
	message: 'Try a different name or clear the search.',
	comment: 'Description text in the theme studio library section.',
});
const IMPORT_A_CSS_FILE_TO_GET_STARTED_OR_DESCRIPTOR = msg({
	message: 'Import a .css file to get started, or paste your own CSS in Quick CSS.',
	comment: 'Description text in the theme studio library section.',
});
const ENABLE_DESCRIPTOR = msg({
	message: 'Enable {themeName}',
	comment: 'Accessible label for the switch that enables a saved theme file.',
});
const NAME_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Short label in the theme studio library section. Keep it concise.',
});
const AUTHOR_DESCRIPTOR = msg({
	message: 'Author',
	comment: 'Short label in the theme studio library section. Keep it concise. Keep the tone plain and specific.',
});
const VERSION_DESCRIPTOR = msg({
	message: 'Version',
	comment: 'Short label in the theme studio library section. Keep it concise.',
});
const TAGS_DESCRIPTOR = msg({
	message: 'Tags',
	comment: 'Short label in the theme studio library section. Keep it concise.',
});
const COMMA_SEPARATED_DESCRIPTOR = msg({
	message: 'comma, separated',
	comment: 'Short label in the theme studio library section. Keep it concise.',
});
const THEME_CSS_DESCRIPTOR = msg({
	message: 'Theme CSS',
	comment: 'Short label in the theme studio library section. Keep it concise.',
});
const NO_THEME_SELECTED_DESCRIPTOR = msg({
	message: 'No theme selected.',
	comment: 'Empty-state text in the theme studio library section.',
});
const PICK_A_THEME_ON_THE_LEFT_TO_EDIT_DESCRIPTOR = msg({
	message: 'Pick a theme on the left to edit its name, metadata, and CSS. Or import a new one.',
	comment: 'Description text in the theme studio library section.',
});

function LibraryCssEditorLoading() {
	return (
		<div
			className={styles.cssEditorLoading}
			aria-hidden="true"
			data-flx="theme-studio.library-section.css-editor-loading"
		/>
	);
}

const LibraryCssEditor = createDefaultLoadableComponent<QuickCssEditorProps>({
	displayName: 'LibraryCssEditor',
	LoadingComponent: LibraryCssEditorLoading,
	load: () => import('@app/features/theme_studio/sections/QuickCssEditor'),
});

interface LibrarySectionProps {
	baseTheme: ThemeStudioBaseTheme;
}

export const LibrarySection: React.FC<LibrarySectionProps> = observer(({baseTheme}) => {
	const {i18n} = useLingui();
	const cssImportRef = useRef<HTMLInputElement | null>(null);
	const libraryImportRef = useRef<HTMLInputElement | null>(null);
	const [selectedThemeId, setSelectedThemeId] = useState<string | null>(null);
	const [draftName, setDraftName] = useState('');
	const [draftDescription, setDraftDescription] = useState('');
	const [draftAuthor, setDraftAuthor] = useState('');
	const [draftVersion, setDraftVersion] = useState('');
	const [draftTags, setDraftTags] = useState('');
	const [draftCss, setDraftCss] = useState('');
	const themeLibraryRevision = ThemeLibrary.revision;
	useEffect(() => {
		void ThemeLibrary.init();
	}, []);
	const searchQuery = ThemeStudioState.librarySearch.trim().toLowerCase();
	const filteredThemes = useMemo(() => {
		if (searchQuery.length === 0) return ThemeLibrary.themes;
		return ThemeLibrary.themes.filter((theme) => {
			return (
				theme.name.toLowerCase().includes(searchQuery) ||
				theme.description.toLowerCase().includes(searchQuery) ||
				theme.tags.some((tag) => tag.toLowerCase().includes(searchQuery)) ||
				theme.fileName.toLowerCase().includes(searchQuery)
			);
		});
	}, [searchQuery, themeLibraryRevision]);
	const selectedTheme = ThemeLibrary.themes.find((theme) => theme.id === selectedThemeId) ?? null;
	useEffect(() => {
		if (selectedThemeId && ThemeLibrary.themes.some((theme) => theme.id === selectedThemeId)) return;
		setSelectedThemeId(ThemeLibrary.themes[0]?.id ?? null);
	}, [selectedThemeId, themeLibraryRevision]);
	useEffect(() => {
		if (!selectedTheme) {
			setDraftName('');
			setDraftDescription('');
			setDraftAuthor('');
			setDraftVersion('');
			setDraftTags('');
			setDraftCss('');
			return;
		}
		setDraftName(selectedTheme.name);
		setDraftDescription(selectedTheme.description);
		setDraftAuthor(selectedTheme.author);
		setDraftVersion(selectedTheme.version);
		setDraftTags(selectedTheme.tags.join(', '));
		setDraftCss(selectedTheme.css);
	}, [selectedTheme?.id, selectedTheme?.updatedAt]);
	const hasDirty = useMemo(() => {
		if (!selectedTheme) return false;
		return (
			draftName.trim() !== selectedTheme.name ||
			draftDescription !== selectedTheme.description ||
			draftAuthor !== selectedTheme.author ||
			draftVersion !== selectedTheme.version ||
			draftTags !== selectedTheme.tags.join(', ') ||
			draftCss !== selectedTheme.css
		);
	}, [selectedTheme, draftName, draftDescription, draftAuthor, draftVersion, draftTags, draftCss]);
	const handleSelect = useCallback((id: string) => setSelectedThemeId(id), []);
	const handleSave = useCallback(() => {
		if (!selectedTheme) return;
		const metadata = {
			name: draftName.trim() || selectedTheme.name,
			description: draftDescription,
			author: draftAuthor,
			version: draftVersion,
			tags: parseTagInput(draftTags),
		};
		void ThemeLibrary.updateThemeDetails(selectedTheme.id, {
			...metadata,
			css: upsertThemeCssHeader(draftCss, metadata),
		}).then(() => {
			broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
			ToastCommands.success(i18n._(THEME_SAVED_DESCRIPTOR));
		});
	}, [selectedTheme, draftName, draftDescription, draftAuthor, draftVersion, draftTags, draftCss, i18n]);
	const handleDuplicate = useCallback(() => {
		if (!selectedTheme) return;
		void ThemeLibrary.duplicateTheme(selectedTheme.id).then((theme) => {
			if (theme) {
				setSelectedThemeId(theme.id);
				broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
			}
		});
	}, [selectedTheme]);
	const handleExportSelected = useCallback(() => {
		if (!selectedTheme) return;
		downloadTextFile(selectedTheme.css, createThemeExportFileName(selectedTheme), 'text/css;charset=utf-8');
	}, [selectedTheme]);
	const handleDelete = useCallback(() => {
		if (!selectedTheme) return;
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_THEME_DESCRIPTOR)}
					description={i18n._(THIS_REMOVES_FROM_THIS_DEVICE_DESCRIPTOR, {themeName: selectedTheme.name})}
					primaryText={i18n._(DELETE_THEME_2_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={() => {
						void ThemeLibrary.deleteTheme(selectedTheme.id).then(() => {
							broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
						});
					}}
					data-flx="theme-studio.library-section.handle-delete.confirm-modal"
				/>
			)),
		);
	}, [selectedTheme, i18n]);
	const handleImportCss = useCallback(
		(event: React.ChangeEvent<HTMLInputElement>) => {
			const files = [...(event.target.files ?? [])];
			event.target.value = '';
			if (files.length === 0) return;
			void ThemeLibrary.importCssFiles(files).then((themes) => {
				broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
				ToastCommands.success(i18n._(IMPORTED_OTHER_DESCRIPTOR, {length: themes.length}));
			});
		},
		[i18n],
	);
	const handleImportDirectory = useCallback(() => {
		void ThemeLibrary.importDesktopThemeDirectory().then((themes) => {
			if (themes.length > 0) {
				broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
				ToastCommands.success(i18n._(IMPORTED_OTHER_2_DESCRIPTOR, {length: themes.length}));
			}
		});
	}, [i18n]);
	const handleImportLibrary = useCallback(
		(event: React.ChangeEvent<HTMLInputElement>) => {
			const file = event.target.files?.[0];
			event.target.value = '';
			if (!file) return;
			void readFileText(file)
				.then((text) => ThemeLibrary.importExportPayload(JSON.parse(text)))
				.then((result) => {
					broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
					ToastCommands.success(
						i18n._(IMPORTED_OTHER_AND_OTHER_DESCRIPTOR, {themes: result.themes, assets: result.assets}),
					);
				})
				.catch(() =>
					showThemeStudioErrorModal(
						i18n,
						() => i18n._(WE_COULDN_T_IMPORT_THAT_THEME_LIBRARY_FILE_DESCRIPTOR),
						'theme-studio.library-section.import-library-error-modal',
					),
				);
		},
		[i18n],
	);
	const handleExportLibrary = useCallback(() => {
		void ThemeLibrary.buildExportPayload().then((payload) => {
			downloadTextFile(JSON.stringify(payload, null, 2), 'fluxer-theme-library.json', 'application/json;charset=utf-8');
		});
	}, []);
	const hasDesktopFileAccess = isDesktop() && Boolean(getElectronAPI()?.pickThemeLocalFiles);
	return (
		<div className={styles.section} data-flx="theme-studio.library-section.section">
			<div className={styles.toolbar} data-flx="theme-studio.library-section.toolbar">
				<div className={styles.toolbarSearch} data-flx="theme-studio.library-section.toolbar-search">
					<StudioSearchInput
						value={ThemeStudioState.librarySearch}
						onChange={(value) => ThemeStudioState.setLibrarySearch(value)}
						placeholder={i18n._(SEARCH_THEMES_DESCRIPTOR)}
						data-flx="theme-studio.library-section.studio-search-input.set-library-search"
					/>
				</div>
				<div className={styles.toolbarActions} data-flx="theme-studio.library-section.toolbar-actions">
					<StudioButton
						variant="secondary"
						compact
						leadingIcon={<FileCssIcon size={13} weight="bold" data-flx="theme-studio.library-section.file-css-icon" />}
						onClick={() => cssImportRef.current?.click()}
						data-flx="theme-studio.library-section.studio-button.click"
					>
						<Trans>Import CSS</Trans>
					</StudioButton>
					{hasDesktopFileAccess ? (
						<StudioButton
							variant="secondary"
							compact
							leadingIcon={
								<FolderOpenIcon size={13} weight="bold" data-flx="theme-studio.library-section.folder-open-icon" />
							}
							onClick={handleImportDirectory}
							data-flx="theme-studio.library-section.studio-button.import-directory"
						>
							<Trans>Import folder</Trans>
						</StudioButton>
					) : null}
					<StudioButton
						variant="secondary"
						compact
						leadingIcon={
							<UploadSimpleIcon size={13} weight="bold" data-flx="theme-studio.library-section.upload-simple-icon" />
						}
						onClick={() => libraryImportRef.current?.click()}
						data-flx="theme-studio.library-section.studio-button.click--2"
					>
						<Trans>Import library</Trans>
					</StudioButton>
					<StudioButton
						variant="secondary"
						compact
						leadingIcon={
							<DownloadSimpleIcon
								size={13}
								weight="bold"
								data-flx="theme-studio.library-section.download-simple-icon"
							/>
						}
						onClick={handleExportLibrary}
						data-flx="theme-studio.library-section.studio-button.export-library"
					>
						<Trans>Export library</Trans>
					</StudioButton>
				</div>
				<input
					ref={cssImportRef}
					type="file"
					accept=".css,text/css,text/plain"
					multiple
					hidden
					onChange={handleImportCss}
					aria-hidden
					data-flx="theme-studio.library-section.input.import-css.file"
				/>
				<input
					ref={libraryImportRef}
					type="file"
					accept=".json,application/json,text/plain"
					hidden
					onChange={handleImportLibrary}
					aria-hidden
					data-flx="theme-studio.library-section.input.import-library.file"
				/>
			</div>
			<StudioSplit
				className={styles.split}
				orientation="horizontal"
				initialSize={280}
				minSize={200}
				maxSize={460}
				storageKey="ThemeStudio:librarySplit"
				first={
					<div className={styles.list} data-flx="theme-studio.library-section.list">
						{filteredThemes.length === 0 ? (
							<div className={styles.listEmpty} data-flx="theme-studio.library-section.list-empty">
								<StudioEmptyState
									icon={
										<FileCssIcon size={20} weight="duotone" data-flx="theme-studio.library-section.file-css-icon--2" />
									}
									title={
										searchQuery.length > 0
											? i18n._(NO_THEMES_MATCH_YOUR_SEARCH_DESCRIPTOR)
											: i18n._(NO_THEMES_SAVED_YET_DESCRIPTOR)
									}
									description={
										searchQuery.length > 0
											? i18n._(TRY_A_DIFFERENT_NAME_OR_CLEAR_THE_SEARCH_DESCRIPTOR)
											: i18n._(IMPORT_A_CSS_FILE_TO_GET_STARTED_OR_DESCRIPTOR)
									}
									data-flx="theme-studio.library-section.studio-empty-state"
								/>
							</div>
						) : (
							filteredThemes.map((theme) => {
								const enabled = ThemeLibrary.enabledThemeIds.includes(theme.id);
								return (
									<div
										key={theme.id}
										className={clsx(styles.listItem, theme.id === selectedThemeId && styles.listItemActive)}
										data-flx="theme-studio.library-section.list-item"
									>
										<FocusRing offset={-2} data-flx="theme-studio.library-section.focus-ring">
											<button
												type="button"
												className={styles.listItemButton}
												aria-pressed={theme.id === selectedThemeId}
												onClick={() => handleSelect(theme.id)}
												data-flx="theme-studio.library-section.list-item-button.select"
											>
												<span className={styles.listItemMain} data-flx="theme-studio.library-section.list-item-main">
													<span className={styles.listItemName} data-flx="theme-studio.library-section.list-item-name">
														{theme.name || theme.fileName}
													</span>
													<span className={styles.listItemMeta} data-flx="theme-studio.library-section.list-item-meta">
														{theme.fileName}
													</span>
												</span>
											</button>
										</FocusRing>
										<div className={styles.enableSwitch} data-flx="theme-studio.library-section.enable-switch">
											<Switch
												compact
												ariaLabel={i18n._(ENABLE_DESCRIPTOR, {themeName: theme.name || theme.fileName})}
												value={enabled}
												onChange={(value) => {
													void ThemeLibrary.setThemeEnabled(theme.id, value).then(() => {
														broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision});
													});
												}}
												data-flx="theme-studio.library-section.switch"
											/>
										</div>
									</div>
								);
							})
						)}
					</div>
				}
				second={
					selectedTheme ? (
						<div className={styles.editor} data-flx="theme-studio.library-section.editor">
							<div className={styles.editorTopBar} data-flx="theme-studio.library-section.editor-top-bar">
								<div className={styles.editorTitle} data-flx="theme-studio.library-section.editor-title">
									{selectedTheme.name || selectedTheme.fileName}
								</div>
								<div className={styles.editorActions} data-flx="theme-studio.library-section.editor-actions">
									<StudioButton
										variant="secondary"
										compact
										leadingIcon={
											<DownloadSimpleIcon
												size={13}
												weight="bold"
												data-flx="theme-studio.library-section.download-simple-icon--2"
											/>
										}
										onClick={handleExportSelected}
										data-flx="theme-studio.library-section.studio-button.export-selected"
									>
										<Trans>Export</Trans>
									</StudioButton>
									<StudioButton
										variant="secondary"
										compact
										leadingIcon={<CopyIcon size={13} weight="bold" data-flx="theme-studio.library-section.copy-icon" />}
										onClick={handleDuplicate}
										data-flx="theme-studio.library-section.studio-button.duplicate"
									>
										<Trans>Duplicate</Trans>
									</StudioButton>
									<StudioButton
										variant="danger"
										compact
										leadingIcon={
											<TrashIcon size={13} weight="bold" data-flx="theme-studio.library-section.trash-icon" />
										}
										onClick={handleDelete}
										data-flx="theme-studio.library-section.studio-button.delete"
									>
										<Trans>Delete</Trans>
									</StudioButton>
								</div>
							</div>
							<div className={styles.fields} data-flx="theme-studio.library-section.fields">
								<LibraryField
									label={i18n._(NAME_DESCRIPTOR)}
									value={draftName}
									onChange={setDraftName}
									data-flx="theme-studio.library-section.library-field.set-draft-name"
								/>
								<LibraryField
									label={i18n._(AUTHOR_DESCRIPTOR)}
									value={draftAuthor}
									onChange={setDraftAuthor}
									data-flx="theme-studio.library-section.library-field.set-draft-author"
								/>
								<LibraryField
									label={i18n._(VERSION_DESCRIPTOR)}
									value={draftVersion}
									onChange={setDraftVersion}
									data-flx="theme-studio.library-section.library-field.set-draft-version"
								/>
								<LibraryField
									label={i18n._(TAGS_DESCRIPTOR)}
									value={draftTags}
									onChange={setDraftTags}
									placeholder={i18n._(COMMA_SEPARATED_DESCRIPTOR)}
									data-flx="theme-studio.library-section.library-field.set-draft-tags"
								/>
								<LibraryField
									label={i18n._(DESCRIPTION_DESCRIPTOR)}
									value={draftDescription}
									onChange={setDraftDescription}
									fullWidth
									data-flx="theme-studio.library-section.library-field.set-draft-description"
								/>
							</div>
							<div className={styles.cssWrap} data-flx="theme-studio.library-section.css-wrap">
								<div className={styles.cssFrame} data-flx="theme-studio.library-section.css-frame">
									<LibraryCssEditor
										ariaLabel={i18n._(THEME_CSS_DESCRIPTOR)}
										baseTheme={baseTheme}
										className={styles.cssEditor}
										value={draftCss}
										onChange={setDraftCss}
										data-flx="theme-studio.library-section.library-css-editor.set-draft-css"
									/>
								</div>
							</div>
							<div className={styles.editorFooter} data-flx="theme-studio.library-section.editor-footer">
								<StudioButton
									variant="primary"
									leadingIcon={
										<FloppyDiskIcon size={13} weight="bold" data-flx="theme-studio.library-section.floppy-disk-icon" />
									}
									disabled={!hasDirty}
									onClick={handleSave}
									data-flx="theme-studio.library-section.studio-button.save"
								>
									<Trans>Save theme</Trans>
								</StudioButton>
								{hasDirty ? (
									<span className={styles.dirtyHint} data-flx="theme-studio.library-section.dirty-hint">
										<Trans>Unsaved changes</Trans>
									</span>
								) : null}
							</div>
						</div>
					) : (
						<div className={styles.editorEmpty} data-flx="theme-studio.library-section.editor-empty">
							<StudioEmptyState
								icon={
									<FilePlusIcon size={20} weight="duotone" data-flx="theme-studio.library-section.file-plus-icon" />
								}
								title={i18n._(NO_THEME_SELECTED_DESCRIPTOR)}
								description={i18n._(PICK_A_THEME_ON_THE_LEFT_TO_EDIT_DESCRIPTOR)}
								data-flx="theme-studio.library-section.studio-empty-state--2"
							/>
						</div>
					)
				}
				data-flx="theme-studio.library-section.split"
			/>
		</div>
	);
});

interface LibraryFieldProps {
	label: string;
	value: string;
	onChange: (value: string) => void;
	placeholder?: string;
	fullWidth?: boolean;
}

const LibraryField: React.FC<LibraryFieldProps> = ({label, value, onChange, placeholder, fullWidth}) => {
	const inputId = useId();
	return (
		<div
			className={clsx(styles.field, fullWidth && styles.fieldFull)}
			data-flx="theme-studio.library-section.library-field.field"
		>
			<label
				className={styles.fieldLabel}
				htmlFor={inputId}
				data-flx="theme-studio.library-section.library-field.field-label"
			>
				{label}
			</label>
			<FocusRing within offset={-1} data-flx="theme-studio.library-section.library-field.focus-ring">
				<input
					id={inputId}
					type="text"
					className={styles.fieldInput}
					value={value}
					placeholder={placeholder}
					onChange={(event) => onChange(event.target.value)}
					data-flx="theme-studio.library-section.library-field.field-input.change.text"
				/>
			</FocusRing>
		</div>
	);
};
