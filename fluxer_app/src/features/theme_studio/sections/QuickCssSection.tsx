// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {createDefaultLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import {ShareThemeModal} from '@app/features/theme/components/modals/ShareThemeModal';
import type {QuickCssEditorProps} from '@app/features/theme_studio/sections/QuickCssEditor';
import styles from '@app/features/theme_studio/sections/QuickCssSection.module.css';
import {broadcastThemeStudioMessage} from '@app/features/theme_studio/state/ThemeStudioBroadcast';
import {StudioButton} from '@app/features/theme_studio/ui/StudioButton';
import {showThemeStudioErrorModal} from '@app/features/theme_studio/utils/ThemeStudioErrorModalUtils';
import type {ThemeStudioBaseTheme} from '@app/features/theme_studio/utils/ThemeStudioPinnedVariables';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {
	copyText,
	downloadTextFile,
	readFileText,
} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {
	ArrowCounterClockwiseIcon,
	CopyIcon,
	DownloadSimpleIcon,
	ShareNetworkIcon,
	UploadSimpleIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const QUICK_CSS_REPLACED_FROM_DESCRIPTOR = msg({
	message: 'Quick CSS replaced from {fileName}.',
	comment: 'Toast after importing a CSS file into the Quick CSS editor.',
});
const WE_COULDN_T_READ_THAT_CSS_FILE_DESCRIPTOR = msg({
	message: "We couldn't read that CSS file.",
	comment: 'Error message in the theme studio quick css section.',
});
const QUICK_CSS_COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Quick CSS copied to clipboard.',
	comment: 'Description text in the theme studio quick css section.',
});
const WE_COULDN_T_COPY_THAT_DESCRIPTOR = msg({
	message: "We couldn't copy that.",
	comment: 'Error message in the theme studio quick css section.',
});
const ADD_SOME_CSS_BEFORE_SHARING_DESCRIPTOR = msg({
	message: 'Add some CSS before sharing.',
	comment: 'Button or menu action label in the theme studio quick css section. Keep it concise.',
});
const CLEAR_QUICK_CSS_DESCRIPTOR = msg({
	message: 'Clear quick CSS?',
	comment: 'Confirmation prompt in the theme studio quick css section.',
});
const THIS_REMOVES_ALL_CUSTOM_CSS_OVERRIDES_THEME_FILES_DESCRIPTOR = msg({
	message: 'This removes all custom CSS overrides. Theme files in your library are not affected.',
	comment: 'Description text in the theme studio quick css section. Keep the tone plain and specific.',
});
const CLEAR_DESCRIPTOR = msg({
	message: 'Clear',
	comment: 'Button or menu action label in the theme studio quick css section. Keep it concise.',
});
const QUICK_CSS_EDITOR_DESCRIPTOR = msg({
	message: 'Quick CSS editor',
	comment: 'Short label in the theme studio quick css section. Keep it concise.',
});

function QuickCssEditorLoading() {
	return (
		<div className={styles.editorLoading} aria-hidden="true" data-flx="theme-studio.quick-css-section.editor-loading" />
	);
}

const QuickCssEditor = createDefaultLoadableComponent<QuickCssEditorProps>({
	displayName: 'QuickCssEditor',
	LoadingComponent: QuickCssEditorLoading,
	load: () => import('@app/features/theme_studio/sections/QuickCssEditor'),
});

interface QuickCssSectionProps {
	baseTheme: ThemeStudioBaseTheme;
}

export const QuickCssSection: React.FC<QuickCssSectionProps> = observer(({baseTheme}) => {
	const {i18n} = useLingui();
	const customThemeCss = Accessibility.customThemeCss ?? '';
	const [draft, setDraft] = useState(customThemeCss);
	const fileInputRef = useRef<HTMLInputElement | null>(null);
	useEffect(() => {
		setDraft(customThemeCss);
	}, [customThemeCss]);
	const flush = useCallback((next: string) => {
		const trimmed = next.trim().length === 0 ? null : next;
		if (trimmed === (Accessibility.customThemeCss ?? null)) return;
		AccessibilityCommands.update({customThemeCss: trimmed});
		broadcastThemeStudioMessage({type: 'customThemeCss', value: trimmed});
	}, []);
	const handleChange = useCallback(
		(next: string) => {
			setDraft(next);
			flush(next);
		},
		[flush],
	);
	const handleImport = (event: React.ChangeEvent<HTMLInputElement>) => {
		const file = event.target.files?.[0];
		event.target.value = '';
		if (!file) return;
		void readFileText(file)
			.then((text) => {
				setDraft(text);
				flush(text);
				ToastCommands.success(i18n._(QUICK_CSS_REPLACED_FROM_DESCRIPTOR, {fileName: file.name}));
			})
			.catch(() =>
				showThemeStudioErrorModal(
					i18n,
					() => i18n._(WE_COULDN_T_READ_THAT_CSS_FILE_DESCRIPTOR),
					'theme-studio.quick-css-section.import-error-modal',
				),
			);
	};
	const handleDownload = () => {
		downloadTextFile(draft, 'quick.css', 'text/css;charset=utf-8');
	};
	const handleCopy = () => {
		void copyText(draft)
			.then(() => ToastCommands.success(i18n._(QUICK_CSS_COPIED_TO_CLIPBOARD_DESCRIPTOR)))
			.catch(() =>
				showThemeStudioErrorModal(
					i18n,
					() => i18n._(WE_COULDN_T_COPY_THAT_DESCRIPTOR),
					'theme-studio.quick-css-section.copy-error-modal',
				),
			);
	};
	const handleShare = () => {
		if (draft.trim().length === 0) {
			showThemeStudioErrorModal(
				i18n,
				() => i18n._(ADD_SOME_CSS_BEFORE_SHARING_DESCRIPTOR),
				'theme-studio.quick-css-section.share-empty-error-modal',
			);
			return;
		}
		ModalCommands.push(
			modal(() => (
				<ShareThemeModal themeCss={draft} data-flx="theme-studio.quick-css-section.handle-share.share-theme-modal" />
			)),
		);
	};
	const handleClear = () => {
		if (draft.trim().length === 0) return;
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(CLEAR_QUICK_CSS_DESCRIPTOR)}
					description={i18n._(THIS_REMOVES_ALL_CUSTOM_CSS_OVERRIDES_THEME_FILES_DESCRIPTOR)}
					primaryText={i18n._(CLEAR_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={() => {
						setDraft('');
						flush('');
					}}
					data-flx="theme-studio.quick-css-section.handle-clear.confirm-modal"
				/>
			)),
		);
	};
	const characterCount = draft.length;
	const lineCount = characterCount === 0 ? 0 : draft.split('\n').length;
	return (
		<div className={styles.section} data-flx="theme-studio.quick-css-section.section">
			<div className={styles.toolbar} data-flx="theme-studio.quick-css-section.toolbar">
				<StudioButton
					variant="secondary"
					compact
					leadingIcon={
						<UploadSimpleIcon size={13} weight="bold" data-flx="theme-studio.quick-css-section.upload-simple-icon" />
					}
					onClick={() => fileInputRef.current?.click()}
					data-flx="theme-studio.quick-css-section.studio-button.click"
				>
					<Trans>Import CSS</Trans>
				</StudioButton>
				<StudioButton
					variant="secondary"
					compact
					leadingIcon={
						<DownloadSimpleIcon
							size={13}
							weight="bold"
							data-flx="theme-studio.quick-css-section.download-simple-icon"
						/>
					}
					onClick={handleDownload}
					data-flx="theme-studio.quick-css-section.studio-button.download"
				>
					<Trans>Download</Trans>
				</StudioButton>
				<StudioButton
					variant="secondary"
					compact
					leadingIcon={<CopyIcon size={13} weight="bold" data-flx="theme-studio.quick-css-section.copy-icon" />}
					onClick={handleCopy}
					data-flx="theme-studio.quick-css-section.studio-button.copy"
				>
					<Trans>Copy</Trans>
				</StudioButton>
				<StudioButton
					variant="primary"
					compact
					leadingIcon={
						<ShareNetworkIcon size={13} weight="bold" data-flx="theme-studio.quick-css-section.share-network-icon" />
					}
					onClick={handleShare}
					data-flx="theme-studio.quick-css-section.studio-button.share"
				>
					<Trans>Share</Trans>
				</StudioButton>
				<div className={styles.toolbarActions} data-flx="theme-studio.quick-css-section.toolbar-actions">
					<StudioButton
						variant="danger"
						compact
						leadingIcon={
							<ArrowCounterClockwiseIcon
								size={13}
								weight="bold"
								data-flx="theme-studio.quick-css-section.arrow-counter-clockwise-icon"
							/>
						}
						onClick={handleClear}
						data-flx="theme-studio.quick-css-section.studio-button.clear"
					>
						<Trans>Clear</Trans>
					</StudioButton>
				</div>
				<input
					ref={fileInputRef}
					type="file"
					accept=".css,text/css,text/plain"
					hidden
					onChange={handleImport}
					aria-hidden
					data-flx="theme-studio.quick-css-section.input.import.file"
				/>
			</div>
			<div className={styles.editorWrap} data-flx="theme-studio.quick-css-section.editor-wrap">
				<div className={styles.editorFrame} data-flx="theme-studio.quick-css-section.editor-frame">
					<QuickCssEditor
						ariaLabel={i18n._(QUICK_CSS_EDITOR_DESCRIPTOR)}
						baseTheme={baseTheme}
						className={styles.editor}
						value={draft}
						onChange={handleChange}
						data-flx="theme-studio.quick-css-section.editor.change"
					/>
				</div>
			</div>
			<div className={styles.footer} data-flx="theme-studio.quick-css-section.footer">
				<span data-flx="theme-studio.quick-css-section.span">
					<Plural value={lineCount} one="# line" other="# lines" data-flx="theme-studio.quick-css-section.plural" />
				</span>
				<span data-flx="theme-studio.quick-css-section.span--2">
					<Plural
						value={characterCount}
						one="# character"
						other="# characters"
						data-flx="theme-studio.quick-css-section.plural--2"
					/>
				</span>
				<span className={styles.spacer} data-flx="theme-studio.quick-css-section.spacer" />
				<span data-flx="theme-studio.quick-css-section.span--3">
					<Trans>Changes apply instantly to the live client.</Trans>
				</span>
			</div>
		</div>
	);
});
