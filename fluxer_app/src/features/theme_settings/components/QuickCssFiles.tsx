// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeCustomThemeCss} from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import Accounts from '@app/features/auth/state/Accounts';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {downloadTextFile} from '@app/features/platform/utils/DownloadFile';
import * as ThemeProfileCommands from '@app/features/theme/commands/ThemeProfileCommands';
import {ShareThemeModal} from '@app/features/theme/components/modals/ShareThemeModal';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import ThemeEditTarget, {type ThemeTarget} from '@app/features/theme/state/ThemeEditTarget';
import ThemeLibrary, {getThemeLabel, type ThemeLibraryTheme} from '@app/features/theme/state/ThemeLibrary';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import {upsertThemeCssHeader} from '@app/features/theme/utils/ThemeCssUtils';
import QuickCssEditor from '@app/features/theme_settings/components/QuickCssEditor';
import {confirmDiscardThemeChanges} from '@app/features/theme_settings/components/QuickCssEditorDiscard';
import styles from '@app/features/theme_settings/components/QuickCssWorkspace.module.css';
import {EditTargetNote} from '@app/features/theme_settings/components/ThisAccountSection';
import EditorPreferences from '@app/features/theme_settings/editor/EditorPreferences';
import {readCachedEditorState, readOpenedCss} from '@app/features/theme_settings/editor/EditorStateCache';
import {useCurrentAccountLabels} from '@app/features/theme_settings/hooks/useCurrentAccountLabels';
import ThemeSettingsState, {QUICK_CSS_EDITOR_FILE} from '@app/features/theme_settings/state/ThemeSettingsState';
import {getLinkedThemeFileLine, isLinkedThemeReadOnly} from '@app/features/theme_settings/utils/LinkedThemeFile';
import {
	showThemeSettingsErrorModal,
	WE_COULDN_T_SAVE_THAT_THEME_DESCRIPTOR,
} from '@app/features/theme_settings/utils/ThemeSettingsErrorModal';
import {
	CUSTOM_CSS_IS_OFF_DESCRIPTOR,
	THEME_SAVED_DESCRIPTOR,
} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {copyText, readFileText} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {QUICK_CSS_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import type {EditorView} from '@codemirror/view';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ArrowCounterClockwiseIcon,
	CopyIcon,
	DownloadSimpleIcon,
	EraserIcon,
	FloppyDiskIcon,
	LinkBreakIcon,
	PlayIcon,
	ShareNetworkIcon,
	UploadSimpleIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useRef, useState} from 'react';

const QUICK_CSS_WRITE_DELAY_MS = 200;
const QUICK_CSS_WRITE_DELAY_STEP_MS = 100;
const QUICK_CSS_WRITE_DELAY_STEP_CHARS = 50 * 1024;

function writeDelayFor(css: string): number {
	return (
		QUICK_CSS_WRITE_DELAY_MS + Math.floor(css.length / QUICK_CSS_WRITE_DELAY_STEP_CHARS) * QUICK_CSS_WRITE_DELAY_STEP_MS
	);
}

const FILE_DESCRIPTOR = msg({
	message: 'File',
	comment: 'Label of the picker in the quick CSS editor window that chooses between quick CSS and a saved theme.',
});
const QUICK_CSS_EDITOR_DESCRIPTOR = msg({
	message: 'Quick CSS editor',
	comment: 'Accessible label for the code editor in the quick CSS editor window.',
});
const THEME_CSS_DESCRIPTOR = msg({
	message: 'Theme CSS',
	comment: 'Accessible label for the code editor while it shows the CSS of a saved theme.',
});
const DESKTOP_QUICK_CSS_LINE_DESCRIPTOR = msg({
	message: 'This is the quick CSS of the desktop app theme.',
	comment:
		'Shown in the quick CSS editor window while it edits the quick CSS shared by every account in the desktop app.',
});
const ACCOUNT_QUICK_CSS_LINE_DESCRIPTOR = msg({
	message: 'This is the quick CSS of {username} on {instanceName}.',
	comment:
		"Shown in the quick CSS editor window while it edits an account's own quick CSS. {username} is the account's username and {instanceName} is its instance.",
});
const CHANGES_SHOW_AS_YOU_TYPE_DESCRIPTOR = msg({
	message: 'Changes show in the app as you type.',
	comment: 'Shown at the bottom of the quick CSS editor window while it edits quick CSS.',
});
const NOT_APPLIED_YET_DESCRIPTOR = msg({
	message: 'Not applied yet',
	comment:
		'Shown in the status bar of the quick CSS editor while "Live preview" is off and there are changes the app does not show yet.',
});
const APPLY_TO_SEE_CHANGES_DESCRIPTOR = msg({
	message: 'Changes show in the app when you select "Apply changes".',
	comment: 'Shown in the status bar of the quick CSS editor while "Live preview" is off.',
});
const APPLY_CHANGES_DESCRIPTOR = msg({
	message: 'Apply changes',
	comment: 'Button in the quick CSS editor that shows the edited quick CSS in the app while "Live preview" is off.',
});
const THEME_PREVIEW_DESCRIPTOR = msg({
	message: 'Changes show in the app as you type. Select "Save theme" to keep them.',
	comment: 'Shown in the status bar of the quick CSS editor while it edits a saved theme that is turned on.',
});
const THEME_OFF_DESCRIPTOR = msg({
	message: 'Turn this theme on to see your changes in the app.',
	comment: 'Shown in the status bar of the quick CSS editor while it edits a saved theme that is turned off.',
});
const USE_THIS_THEME_DESCRIPTOR = msg({
	message: 'Use this theme',
	comment:
		'Menu item in the quick CSS editor while it shows a saved theme. When checked, the theme applies to the app, the same as its switch in Settings > Themes.',
});
const QUICK_CSS_REPLACED_FROM_DESCRIPTOR = msg({
	message: 'Quick CSS replaced from {fileName}.',
	comment: 'Toast after importing a CSS file into the quick CSS editor.',
});
const WE_COULDN_T_READ_THAT_CSS_FILE_DESCRIPTOR = msg({
	message: "We couldn't read that CSS file.",
	comment: 'Error message in the quick CSS editor window.',
});
const QUICK_CSS_COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Quick CSS copied to clipboard.',
	comment: 'Toast after copying quick CSS in the quick CSS editor window.',
});
const WE_COULDN_T_COPY_THAT_DESCRIPTOR = msg({
	message: "We couldn't copy that.",
	comment: 'Error message in the quick CSS editor window.',
});
const ADD_SOME_CSS_BEFORE_SHARING_DESCRIPTOR = msg({
	message: 'Add some CSS before sharing.',
	comment: 'Error message in the quick CSS editor window when sharing empty quick CSS.',
});
const UNDO_ALL_CHANGES_DESCRIPTOR = msg({
	message: 'Undo all changes',
	comment:
		'Button in the quick CSS editor window that puts quick CSS back to how it was when the editor window opened on it.',
});
const QUICK_CSS_RESTORED_DESCRIPTOR = msg({
	message: 'Quick CSS is back to how it was when you opened the editor.',
	comment: 'Toast after pressing "Undo all changes" in the quick CSS editor window.',
});
const CLEAR_QUICK_CSS_DESCRIPTOR = msg({
	message: 'Clear quick CSS?',
	comment: 'Confirmation title in the quick CSS editor window.',
});
const CLEAR_QUICK_CSS_DESCRIPTION_DESCRIPTOR = msg({
	message: 'This removes all of your quick CSS, including color and font changes. Saved themes are not affected.',
	comment: 'Confirmation text in the quick CSS editor window before clearing quick CSS.',
});
const UNSAVED_CHANGES_DESCRIPTOR = msg({
	message: 'Unsaved changes',
	comment: 'Shown in the status bar of the quick CSS editor while a saved theme has changes that are not saved yet.',
});
const CLEAR_DESCRIPTOR = msg({
	message: 'Clear',
	comment: 'Button in the quick CSS editor window that deletes all quick CSS.',
});

interface EditorFileProps {
	filePicker: React.ReactNode;
}

interface PendingQuickCssWrite {
	target: ThemeTarget;
	accountKey: string | null;
	css: string | null;
}

function replaceEditorDocument(view: EditorView | null, next: string): void {
	if (!view || view.state.doc.toString() === next) return;
	view.dispatch({changes: {from: 0, to: view.state.doc.length, insert: next}, userEvent: 'input.replace'});
}

interface QuickCssFileProps extends EditorFileProps {
	target: ThemeTarget;
	sourceKey: string;
}

const QuickCssFile: React.FC<QuickCssFileProps> = observer(({filePicker, target, sourceKey}) => {
	const {i18n} = useLingui();
	const stored = ThemeProfile.customThemeCssFor(target);
	const [cached] = useState(() => readCachedEditorState(sourceKey, stored ?? ''));
	const [draft, setDraft] = useState(cached?.doc ?? stored ?? '');
	const [openedValue] = useState(() => readOpenedCss(sourceKey, stored ?? ''));
	const viewRef = useRef<EditorView | null>(null);
	const pendingRef = useRef<PendingQuickCssWrite | null>(null);
	const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
	const flushRef = useRef(() => {
		if (timerRef.current !== null) {
			clearTimeout(timerRef.current);
			timerRef.current = null;
		}
		const pending = pendingRef.current;
		pendingRef.current = null;
		if (!pending) return;
		if (pending.target === 'account' && pending.accountKey !== Accounts.currentAccountKey) return;
		if (pending.css !== ThemeProfile.customThemeCssFor(pending.target)) {
			ThemeProfileCommands.setCustomThemeCss(pending.css, pending.target);
		}
	});
	const fileInputRef = useRef<HTMLInputElement | null>(null);
	useEffect(() => {
		const flush = flushRef.current;
		ThemeSettingsState.setQuickCssWriteFlusher(flush);
		return () => {
			flush();
			if (ThemeSettingsState.quickCssWriteFlusher === flush) ThemeSettingsState.setQuickCssWriteFlusher(null);
		};
	}, []);
	const lastStoredRef = useRef(stored);
	useEffect(() => {
		const previous = lastStoredRef.current;
		lastStoredRef.current = stored;
		if (pendingRef.current) return;
		setDraft((current) => {
			const normalized = normalizeCustomThemeCss(current);
			if (normalized === stored) return current;
			return normalized === previous ? (stored ?? '') : current;
		});
	}, [stored]);
	const livePreview = EditorPreferences.livePreview;
	const write = (next: string, immediate = true) => {
		setDraft(next);
		pendingRef.current = {target, accountKey: Accounts.currentAccountKey, css: normalizeCustomThemeCss(next)};
		if (timerRef.current !== null) clearTimeout(timerRef.current);
		if (immediate) {
			flushRef.current();
			return;
		}
		timerRef.current = setTimeout(() => flushRef.current(), writeDelayFor(next));
	};
	const edit = (next: string, immediate: boolean) => {
		if (EditorPreferences.livePreview) {
			write(next, immediate);
			return;
		}
		setDraft(next);
	};
	const replace = (next: string) => {
		replaceEditorDocument(viewRef.current, next);
		write(next);
	};
	const unapplied = normalizeCustomThemeCss(draft) !== stored;
	useEffect(() => {
		if (livePreview && unapplied) write(draft);
	}, [livePreview]);
	const handleImport = (event: React.ChangeEvent<HTMLInputElement>) => {
		const file = event.target.files?.[0];
		event.target.value = '';
		if (!file) return;
		void readFileText(file)
			.then((text) => {
				replace(text);
				ToastCommands.success(i18n._(QUICK_CSS_REPLACED_FROM_DESCRIPTOR, {fileName: file.name}));
			})
			.catch(() =>
				showThemeSettingsErrorModal(
					i18n,
					() => i18n._(WE_COULDN_T_READ_THAT_CSS_FILE_DESCRIPTOR),
					'theme-settings.quick-css-editor-window.import-error-modal',
				),
			);
	};
	const handleCopy = () => {
		void copyText(draft)
			.then(() => ToastCommands.success(i18n._(QUICK_CSS_COPIED_TO_CLIPBOARD_DESCRIPTOR)))
			.catch(() =>
				showThemeSettingsErrorModal(
					i18n,
					() => i18n._(WE_COULDN_T_COPY_THAT_DESCRIPTOR),
					'theme-settings.quick-css-editor-window.copy-error-modal',
				),
			);
	};
	const handleShare = () => {
		if (draft.trim().length === 0) {
			showThemeSettingsErrorModal(
				i18n,
				() => i18n._(ADD_SOME_CSS_BEFORE_SHARING_DESCRIPTOR),
				'theme-settings.quick-css-editor-window.share-empty-error-modal',
			);
			return;
		}
		ModalCommands.push(
			modal(() => (
				<ShareThemeModal
					themeCss={draft}
					data-flx="theme-settings.quick-css-editor-window.handle-share.share-theme-modal"
				/>
			)),
		);
	};
	const handleUndoAll = () => {
		replace(openedValue);
		ToastCommands.success(i18n._(QUICK_CSS_RESTORED_DESCRIPTOR));
	};
	const handleClear = () => {
		if (draft.trim().length === 0) return;
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(CLEAR_QUICK_CSS_DESCRIPTOR)}
					description={i18n._(CLEAR_QUICK_CSS_DESCRIPTION_DESCRIPTOR)}
					primaryText={i18n._(CLEAR_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={() => replace('')}
					data-flx="theme-settings.quick-css-editor-window.handle-clear.confirm-modal"
				/>
			)),
		);
	};
	let previewStatus: string | null = null;
	if (ThemeSettingsState.customCssOff) {
		previewStatus = i18n._(CUSTOM_CSS_IS_OFF_DESCRIPTOR);
	} else if (!livePreview) {
		previewStatus = unapplied ? i18n._(NOT_APPLIED_YET_DESCRIPTOR) : i18n._(APPLY_TO_SEE_CHANGES_DESCRIPTOR);
	} else if (target === ThemeProfile.activeTarget) {
		previewStatus = i18n._(CHANGES_SHOW_AS_YOU_TYPE_DESCRIPTOR);
	}
	const menuIcon = (IconComponent: typeof CopyIcon) => (
		<IconComponent size={remFromPx(16)} data-flx="theme-settings.quick-css-files.quick-css-file.menu-icon" />
	);
	const renderMenu = (onClose: () => void) => {
		const select = (action: () => void) => () => {
			onClose();
			action();
		};
		return (
			<>
				<MenuGroup data-flx="theme-settings.quick-css-files.quick-css-file.menu-file">
					<MenuItem
						icon={menuIcon(UploadSimpleIcon)}
						onClick={select(() => fileInputRef.current?.click())}
						data-flx="theme-settings.quick-css-files.quick-css-file.menu-import"
					>
						<Trans>Import CSS</Trans>
					</MenuItem>
					<MenuItem
						icon={menuIcon(DownloadSimpleIcon)}
						onClick={select(() => downloadTextFile(draft, 'quick.css', 'text/css;charset=utf-8'))}
						data-flx="theme-settings.quick-css-files.quick-css-file.menu-download"
					>
						<Trans>Download</Trans>
					</MenuItem>
					<MenuItem
						icon={menuIcon(CopyIcon)}
						onClick={select(handleCopy)}
						data-flx="theme-settings.quick-css-files.quick-css-file.menu-copy"
					>
						<Trans>Copy</Trans>
					</MenuItem>
					<MenuItem
						icon={menuIcon(ShareNetworkIcon)}
						onClick={select(handleShare)}
						data-flx="theme-settings.quick-css-files.quick-css-file.menu-share"
					>
						<Trans>Share</Trans>
					</MenuItem>
				</MenuGroup>
				<MenuGroup data-flx="theme-settings.quick-css-files.quick-css-file.menu-reset">
					<MenuItem
						icon={menuIcon(ArrowCounterClockwiseIcon)}
						disabled={draft === openedValue}
						onClick={select(handleUndoAll)}
						data-flx="theme-settings.quick-css-files.quick-css-file.menu-undo-all"
					>
						{i18n._(UNDO_ALL_CHANGES_DESCRIPTOR)}
					</MenuItem>
					<MenuItem
						icon={menuIcon(EraserIcon)}
						danger
						disabled={draft.trim().length === 0}
						onClick={select(handleClear)}
						data-flx="theme-settings.quick-css-files.quick-css-file.menu-clear"
					>
						{i18n._(CLEAR_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			</>
		);
	};
	return (
		<>
			<QuickCssEditor
				ariaLabel={i18n._(QUICK_CSS_EDITOR_DESCRIPTOR)}
				value={draft}
				onChange={edit}
				onSave={() => write(draft)}
				cacheKey={sourceKey}
				cachedState={cached}
				readStoredCss={() => ThemeProfile.customThemeCssFor(target) ?? ''}
				viewRef={viewRef}
				filePicker={filePicker}
				primaryAction={
					livePreview ? null : (
						<Button
							small
							leftIcon={
								<PlayIcon size={remFromPx(16)} data-flx="theme-settings.quick-css-files.quick-css-file.play-icon" />
							}
							disabled={!unapplied}
							onClick={() => write(draft)}
							data-flx="theme-settings.quick-css-files.quick-css-file.button.apply"
						>
							{i18n._(APPLY_CHANGES_DESCRIPTOR)}
						</Button>
					)
				}
				renderMenu={renderMenu}
				notes={<EditTargetNote data-flx="theme-settings.quick-css-files.quick-css-file.edit-target-note" />}
				previewStatus={previewStatus}
				data-flx="theme-settings.quick-css-files.quick-css-file.editor"
			/>
			<input
				ref={fileInputRef}
				type="file"
				accept=".css,text/css,text/plain"
				hidden
				onChange={handleImport}
				aria-hidden
				data-flx="theme-settings.quick-css-editor-window.quick-css-file.input.import.file"
			/>
		</>
	);
});

const ThemeFile: React.FC<EditorFileProps & {theme: ThemeLibraryTheme}> = observer(({filePicker, theme}) => {
	const {i18n} = useLingui();
	const cacheKey = `theme:${theme.id}`;
	const [cached] = useState(() => readCachedEditorState(cacheKey, theme.css));
	const [draft, setDraft] = useState(cached?.doc ?? theme.css);
	const [saving, setSaving] = useState(false);
	const readOnly = isLinkedThemeReadOnly(theme);
	const linkedLine = getLinkedThemeFileLine(i18n, theme);
	const dirty = !readOnly && draft !== theme.css;
	const savedCssRef = useRef(theme.css);
	useEffect(() => {
		const previousCss = savedCssRef.current;
		savedCssRef.current = theme.css;
		setDraft((current) =>
			current === previousCss || current === theme.css ? theme.css : upsertThemeCssHeader(current, theme),
		);
	}, [theme.id, theme.updatedAt]);
	useEffect(() => {
		ThemeSettingsState.setEditorDirty(dirty);
	}, [dirty]);
	useEffect(() => {
		ThemeLibrary.setFocusedThemeId(theme.id);
		return () => {
			ThemeLibrary.setFocusedThemeId(null);
			ThemeSettingsState.setEditorDirty(false);
			ThemeSettingsState.setThemePreview(null);
		};
	}, [theme.id]);
	const enabled = ThemeProfile.enabledThemeIds.includes(theme.id);
	const previewing = enabled && dirty && EditorPreferences.livePreview;
	useEffect(() => {
		if (!previewing) {
			ThemeSettingsState.setThemePreview(null);
			return;
		}
		const timer = setTimeout(
			() => ThemeSettingsState.setThemePreview({id: theme.id, css: draft}),
			writeDelayFor(draft),
		);
		return () => clearTimeout(timer);
	}, [previewing, draft, theme.id]);
	const handleSave = () => {
		if (!dirty || saving) return;
		setSaving(true);
		void ThemeLibrary.updateThemeDetails(theme.id, {css: draft})
			.then(() => ToastCommands.success(i18n._(THEME_SAVED_DESCRIPTOR)))
			.catch(() =>
				showThemeSettingsErrorModal(
					i18n,
					() => i18n._(WE_COULDN_T_SAVE_THAT_THEME_DESCRIPTOR),
					'theme-settings.quick-css-editor-window.save-error-modal',
				),
			)
			.finally(() => setSaving(false));
	};
	const canUnlink = linkedLine !== null && !ThemeLibrary.isWatchedFolderTheme(theme);
	const renderMenu = (onClose: () => void) => (
		<MenuGroup data-flx="theme-settings.quick-css-files.theme-file.menu">
			<CheckboxItem
				checked={ThemeProfile.enabledThemeIds.includes(theme.id)}
				onCheckedChange={(value) => {
					void ThemeProfileCommands.setThemeEnabled(theme.id, value, ThemeEditTarget.target);
				}}
				data-flx="theme-settings.quick-css-files.theme-file.menu-enabled"
			>
				{i18n._(USE_THIS_THEME_DESCRIPTOR)}
			</CheckboxItem>
			{canUnlink ? (
				<MenuItem
					icon={
						<LinkBreakIcon size={remFromPx(16)} data-flx="theme-settings.quick-css-files.theme-file.link-break-icon" />
					}
					onClick={() => {
						onClose();
						void ThemeLibrary.unlinkTheme(theme.id);
					}}
					data-flx="theme-settings.quick-css-files.theme-file.menu-unlink"
				>
					<Trans comment="Button that stops a theme following its CSS file on disk and turns it back into an editable copy.">
						Unlink
					</Trans>
				</MenuItem>
			) : null}
		</MenuGroup>
	);
	let previewStatus: string | null = null;
	if (ThemeSettingsState.customCssOff) previewStatus = i18n._(CUSTOM_CSS_IS_OFF_DESCRIPTOR);
	else if (readOnly) previewStatus = null;
	else if (!enabled) previewStatus = i18n._(THEME_OFF_DESCRIPTOR);
	else if (EditorPreferences.livePreview) previewStatus = i18n._(THEME_PREVIEW_DESCRIPTOR);
	else if (dirty) previewStatus = i18n._(UNSAVED_CHANGES_DESCRIPTOR);
	return (
		<QuickCssEditor
			ariaLabel={i18n._(THEME_CSS_DESCRIPTOR)}
			value={draft}
			onChange={setDraft}
			onSave={handleSave}
			readOnly={readOnly}
			cacheKey={cacheKey}
			cachedState={cached}
			readStoredCss={() => theme.css}
			filePicker={filePicker}
			primaryAction={
				readOnly ? null : (
					<Button
						small
						leftIcon={
							<FloppyDiskIcon
								size={remFromPx(16)}
								data-flx="theme-settings.quick-css-editor-window.theme-file.floppy-disk-icon"
							/>
						}
						disabled={!dirty}
						submitting={saving}
						onClick={handleSave}
						data-flx="theme-settings.quick-css-editor-window.theme-file.button.save"
					>
						<Trans>Save theme</Trans>
					</Button>
				)
			}
			renderMenu={renderMenu}
			notes={
				linkedLine ? (
					<p className={styles.line} data-flx="theme-settings.quick-css-editor-window.theme-file.line">
						{linkedLine}
					</p>
				) : null
			}
			previewStatus={previewStatus}
			data-flx="theme-settings.quick-css-files.theme-file.editor"
		/>
	);
});

export const QuickCssFiles: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {username, instanceName} = useCurrentAccountLabels();
	const file = ThemeSettingsState.editorFile;
	const target = ThemeEditTarget.target;
	const sourceKey = `quick-css:${target}:${Accounts.currentAccountKey ?? ''}`;
	const theme =
		file === QUICK_CSS_EDITOR_FILE ? null : (ThemeLibrary.themes.find((candidate) => candidate.id === file) ?? null);
	useEffect(() => {
		void ThemeLibrary.init();
	}, []);
	useEffect(() => {
		if (file !== QUICK_CSS_EDITOR_FILE && theme === null && ThemeLibrary.isHydrated) {
			ThemeSettingsState.setEditorFile(QUICK_CSS_EDITOR_FILE);
		}
	}, [file, theme]);
	useEffect(() => {
		ThemeSettingsState.setEditorFileRequestHandler((requested) => {
			if (!ThemeSettingsState.editorDirty) {
				ThemeSettingsState.setEditorFile(requested);
				return;
			}
			confirmDiscardThemeChanges(i18n, () => ThemeSettingsState.setEditorFile(requested));
		});
		return () => ThemeSettingsState.setEditorFileRequestHandler(null);
	}, [i18n]);
	const fileOptions = useMemo(
		(): ReadonlyArray<ComboboxOption<string>> => [
			{value: QUICK_CSS_EDITOR_FILE, label: i18n._(QUICK_CSS_SECTION_DESCRIPTOR)},
			...ThemeLibrary.themes.map((candidate) => ({value: candidate.id, label: getThemeLabel(candidate)})),
		],
		[i18n.locale, ThemeLibrary.themes, ThemeLibrary.revision],
	);
	let description: string | null = null;
	if (!theme) {
		description =
			target === 'desktop'
				? i18n._(DESKTOP_QUICK_CSS_LINE_DESCRIPTOR)
				: i18n._(ACCOUNT_QUICK_CSS_LINE_DESCRIPTOR, {username, instanceName});
	}
	const combobox = (
		<Combobox
			className={styles.fileCombobox}
			aria-label={i18n._(FILE_DESCRIPTOR)}
			value={file}
			options={fileOptions}
			onChange={(value) => ThemeSettingsState.requestEditorFile(value)}
			isSearchable={fileOptions.length > 8}
			density="compact"
			data-flx="theme-settings.quick-css-editor-window.file-combobox.request-editor-file"
		/>
	);
	const filePicker = (
		<div className={styles.filePicker} data-flx="theme-settings.quick-css-editor-window.file-picker">
			{description ? (
				<Tooltip text={description} position="bottom" data-flx="theme-settings.quick-css-files.file-tooltip">
					<div className={styles.fileTooltipTarget} data-flx="theme-settings.quick-css-files.file-tooltip-target">
						{combobox}
					</div>
				</Tooltip>
			) : (
				combobox
			)}
		</div>
	);
	return theme ? (
		<ThemeFile
			key={theme.id}
			theme={theme}
			filePicker={filePicker}
			data-flx="theme-settings.quick-css-editor-window.theme-file"
		/>
	) : (
		<QuickCssFile
			key={sourceKey}
			target={target}
			sourceKey={sourceKey}
			filePicker={filePicker}
			data-flx="theme-settings.quick-css-editor-window.quick-css-file"
		/>
	);
});
