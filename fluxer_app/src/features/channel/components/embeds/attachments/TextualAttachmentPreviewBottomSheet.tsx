// SPDX-License-Identifier: AGPL-3.0-or-later

import {CsvAttachmentTablePanel} from '@app/features/channel/components/embeds/attachments/CsvAttachmentTablePanel';
import {TextualAttachmentCodePanel} from '@app/features/channel/components/embeds/attachments/TextualAttachmentCodePanel';
import styles from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreviewBottomSheet.module.css';
import {
	getAttachmentFileName,
	getLineCount,
	type TextualAttachmentPreviewModalProps,
} from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreviewUtils';
import {DOWNLOAD_DESCRIPTOR, MORE_OPTIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import TextualPreview from '@app/features/messaging/state/TextualPreview';
import {downloadFile} from '@app/features/messaging/utils/FileDownloadUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {MenuBottomSheet, type MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DotsThreeIcon, DownloadSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';

const WRAP_TEXT_DESCRIPTOR = msg({
	message: 'Wrap text',
	comment: 'Short label in the channel and chat textual attachment preview bottom sheet. Keep it concise.',
});
const ATTACHMENT_PREVIEW_DESCRIPTOR = msg({
	message: 'Attachment preview',
	comment: 'Short label in the channel and chat textual attachment preview bottom sheet. Keep it concise.',
});
const OPTIONS_DESCRIPTOR = msg({
	message: 'Options',
	comment: 'Short label in the channel and chat textual attachment preview bottom sheet. Keep it concise.',
});
export const TextualAttachmentPreviewBottomSheet = observer(function TextualAttachmentPreviewBottomSheet({
	attachment,
	csvRows,
	highlightedHtml,
	onClose,
	previewError,
	renderMode = 'code',
	status,
	textContent,
	wrapText,
}: TextualAttachmentPreviewModalProps) {
	const {i18n} = useLingui();
	const [moreOptionsOpen, setMoreOptionsOpen] = useState(false);
	const lineCount = useMemo(
		() => (renderMode === 'csv' ? (csvRows?.length ?? 0) : getLineCount(textContent)),
		[csvRows, renderMode, textContent],
	);
	const visibleLineCount = useMemo(() => Math.max(lineCount, 1), [lineCount]);
	const fileName = getAttachmentFileName(attachment);
	let fileSizeLabel = '';
	if (typeof attachment.size === 'number') {
		fileSizeLabel = formatFileSize(i18n.locale, attachment.size);
	}
	const handleDownload = useCallback(async () => {
		const downloadUrl = attachment.proxy_url ?? attachment.url;
		if (!downloadUrl) {
			return;
		}
		await downloadFile(downloadUrl, 'file', fileName || 'file');
	}, [attachment.proxy_url, attachment.url, fileName]);
	const handleOpenMoreOptions = useCallback(() => {
		setMoreOptionsOpen(true);
	}, []);
	const handleCloseMoreOptions = useCallback(() => {
		setMoreOptionsOpen(false);
	}, []);
	const moreOptionsGroups = useMemo<Array<MenuGroupType>>(() => {
		const items: MenuGroupType['items'] = [
			{
				icon: (
					<DownloadSimpleIcon
						size={20}
						data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.more-options-groups.download-simple-icon"
					/>
				),
				label: i18n._(DOWNLOAD_DESCRIPTOR),
				onClick: handleDownload,
			},
		];
		if (renderMode !== 'csv') {
			items.push({
				icon: undefined,
				label: i18n._(WRAP_TEXT_DESCRIPTOR),
				checked: wrapText,
				onChange: TextualPreview.toggleWrapText,
			});
		}
		return [{items}];
	}, [handleDownload, i18n.locale, renderMode, wrapText]);
	return (
		<>
			<BottomSheet
				isOpen
				onClose={onClose}
				snapPoints={[0, 1]}
				initialSnap={1}
				title={i18n._(ATTACHMENT_PREVIEW_DESCRIPTOR)}
				disablePadding
				data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.bottom-sheet"
			>
				<div
					className={styles.container}
					data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.container"
				>
					<div
						className={styles.codeContainer}
						data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.code-container"
					>
						{renderMode === 'csv' ? (
							<CsvAttachmentTablePanel
								canExpand
								copyTextContent={textContent}
								fillAvailableSpace
								isExpanded
								previewError={previewError}
								rows={csvRows ?? null}
								status={status}
								visibleLineCount={visibleLineCount}
								wrapperClassName={styles.codeSurface}
								data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.csv-attachment-table-panel"
							/>
						) : (
							<TextualAttachmentCodePanel
								canExpand
								fillAvailableSpace
								highlightedHtml={highlightedHtml}
								isExpanded
								previewError={previewError}
								status={status}
								textContent={textContent}
								visibleLineCount={visibleLineCount}
								wrapText={wrapText}
								wrapperClassName={styles.codeSurface}
								data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.textual-attachment-code-panel"
							/>
						)}
					</div>
					<div
						className={styles.footer}
						data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.footer"
					>
						<div
							className={styles.fileSection}
							data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.file-section"
						>
							<div
								className={styles.fileMeta}
								data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.file-meta"
							>
								<span
									className={styles.fileName}
									data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.file-name"
								>
									{fileName}
								</span>
								{fileSizeLabel && (
									<span
										className={styles.fileSize}
										data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.file-size"
									>
										{fileSizeLabel}
									</span>
								)}
							</div>
						</div>
						<div
							className={styles.footerActions}
							data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.footer-actions"
						>
							<button
								type="button"
								className={styles.actionButton}
								onClick={handleDownload}
								aria-label={i18n._(DOWNLOAD_DESCRIPTOR)}
								data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.action-button.download"
							>
								<DownloadSimpleIcon
									size={20}
									weight="regular"
									data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.download-simple-icon"
								/>
							</button>
							<button
								type="button"
								className={styles.actionButton}
								onClick={handleOpenMoreOptions}
								aria-label={i18n._(MORE_OPTIONS_DESCRIPTOR)}
								data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.action-button.open-more-options"
							>
								<DotsThreeIcon
									size={20}
									weight="bold"
									data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.dots-three-icon"
								/>
							</button>
						</div>
					</div>
				</div>
			</BottomSheet>
			<MenuBottomSheet
				isOpen={moreOptionsOpen}
				onClose={handleCloseMoreOptions}
				title={i18n._(OPTIONS_DESCRIPTOR)}
				groups={moreOptionsGroups}
				data-flx="channel.embeds.attachments.textual-attachment-preview-bottom-sheet.menu-bottom-sheet"
			/>
		</>
	);
});
