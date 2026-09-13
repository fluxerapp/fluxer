// SPDX-License-Identifier: AGPL-3.0-or-later

import {TextualAttachmentLanguageCombobox} from '@app/features/channel/components/embeds/attachments/TextualAttachmentLanguageCombobox';
import styles from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreview.module.css';
import {
	getAttachmentFileName,
	type TextualAttachmentPreviewFooterProps,
} from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreviewUtils';
import {MORE_OPTIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowsOutIcon, CaretDownIcon, DotsThreeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';

const VIEW_WHOLE_FILE_DESCRIPTOR = msg({
	message: 'View whole file',
	comment: 'Button or menu action label in the channel and chat textual attachment preview footer. Keep it concise.',
});

export function TextualAttachmentPreviewFooter({
	attachment,
	canExpand,
	countKind = 'line',
	inferredLanguageCode,
	isExpanded,
	lineCount,
	onMoreOptions,
	onOpenFullscreen,
	onSelectLanguage,
	onToggleExpanded,
	selectedLanguage,
	showExpandButton = true,
	showLanguageButton = true,
}: TextualAttachmentPreviewFooterProps) {
	const {i18n} = useLingui();
	const fileName = getAttachmentFileName(attachment);
	let fileSizeLabel = '';
	if (typeof attachment.size === 'number') {
		fileSizeLabel = formatFileSize(i18n.locale, attachment.size);
	}
	let expandButtonLabel: string;
	if (countKind === 'row') {
		expandButtonLabel = isExpanded
			? plural(
					{count: lineCount},
					{
						one: 'Collapse (# row)',
						other: 'Collapse (# rows)',
					},
				)
			: plural(
					{count: lineCount},
					{
						one: 'Expand (# row)',
						other: 'Expand (# rows)',
					},
				);
	} else {
		expandButtonLabel = isExpanded
			? plural(
					{count: lineCount},
					{
						one: 'Collapse (# line)',
						other: 'Collapse (# lines)',
					},
				)
			: plural(
					{count: lineCount},
					{
						one: 'Expand (# line)',
						other: 'Expand (# lines)',
					},
				);
	}
	const fullscreenButtonLabel = i18n._(VIEW_WHOLE_FILE_DESCRIPTOR);
	const moreButtonLabel = i18n._(MORE_OPTIONS_DESCRIPTOR);
	return (
		<div className={styles.footer} data-flx="channel.embeds.attachments.textual-attachment-preview-footer.footer">
			{showExpandButton && canExpand && onToggleExpanded && (
				<Tooltip
					text={expandButtonLabel}
					data-flx="channel.embeds.attachments.textual-attachment-preview-footer.tooltip"
				>
					<FocusRing offset={-2} data-flx="channel.embeds.attachments.textual-attachment-preview-footer.focus-ring">
						<button
							type="button"
							className={styles.expandButton}
							onClick={onToggleExpanded}
							aria-label={expandButtonLabel}
							aria-expanded={isExpanded}
							data-flx="channel.embeds.attachments.textual-attachment-preview-footer.expand-button.toggle-expanded"
						>
							<CaretDownIcon
								size={18}
								weight="bold"
								className={clsx(styles.expandIcon, isExpanded && styles.expandIconExpanded)}
								data-flx="channel.embeds.attachments.textual-attachment-preview-footer.expand-icon"
							/>
						</button>
					</FocusRing>
				</Tooltip>
			)}
			<div
				className={styles.fileSection}
				data-flx="channel.embeds.attachments.textual-attachment-preview-footer.file-section"
			>
				<div
					className={styles.fileMeta}
					data-flx="channel.embeds.attachments.textual-attachment-preview-footer.file-meta"
				>
					<span
						className={styles.fileName}
						data-flx="channel.embeds.attachments.textual-attachment-preview-footer.file-name"
					>
						{fileName}
					</span>
					{fileSizeLabel && (
						<span
							className={styles.fileSize}
							data-flx="channel.embeds.attachments.textual-attachment-preview-footer.file-size"
						>
							{fileSizeLabel}
						</span>
					)}
				</div>
			</div>
			<div
				className={styles.footerActions}
				data-flx="channel.embeds.attachments.textual-attachment-preview-footer.footer-actions"
			>
				{showLanguageButton && (
					<TextualAttachmentLanguageCombobox
						defaultSearchQuery={inferredLanguageCode}
						onSelectLanguage={onSelectLanguage}
						selectedLanguage={selectedLanguage}
						data-flx="channel.embeds.attachments.textual-attachment-preview-footer.textual-attachment-language-combobox"
					/>
				)}
				{onOpenFullscreen && (
					<Tooltip
						text={fullscreenButtonLabel}
						data-flx="channel.embeds.attachments.textual-attachment-preview-footer.tooltip--2"
					>
						<FocusRing
							offset={-2}
							data-flx="channel.embeds.attachments.textual-attachment-preview-footer.focus-ring--3"
						>
							<button
								type="button"
								className={styles.controlButton}
								onClick={onOpenFullscreen}
								aria-label={fullscreenButtonLabel}
								data-flx="channel.embeds.attachments.textual-attachment-preview-footer.control-button.open-fullscreen"
							>
								<ArrowsOutIcon
									size={18}
									weight="regular"
									data-flx="channel.embeds.attachments.textual-attachment-preview-footer.arrows-out-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
				)}
				<Tooltip
					text={moreButtonLabel}
					data-flx="channel.embeds.attachments.textual-attachment-preview-footer.tooltip--3"
				>
					<FocusRing offset={-2} data-flx="channel.embeds.attachments.textual-attachment-preview-footer.focus-ring--4">
						<button
							type="button"
							className={styles.controlButton}
							onClick={onMoreOptions}
							aria-label={moreButtonLabel}
							data-flx="channel.embeds.attachments.textual-attachment-preview-footer.control-button.more-options"
						>
							<DotsThreeIcon
								size={18}
								weight="bold"
								data-flx="channel.embeds.attachments.textual-attachment-preview-footer.dots-three-icon"
							/>
						</button>
					</FocusRing>
				</Tooltip>
			</div>
		</div>
	);
}
