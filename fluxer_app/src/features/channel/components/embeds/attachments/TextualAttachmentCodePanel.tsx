// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreview.module.css';
import {
	PREVIEW_LIMIT_KB,
	type PreviewPanelStyle,
	type TextualAttachmentCodePanelProps,
} from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreviewUtils';
import {COPY_TEXT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {CopyButton} from '@app/features/ui/components/CopyButton';
import {Spinner} from '@app/features/ui/components/Spinner';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {useMemo} from 'react';

const FILE_IS_TOO_LARGE_FOR_INLINE_PREVIEW_LIMIT_DESCRIPTOR = msg({
	message: 'File is too large for inline preview (limit {previewLimitKb} KB).',
	comment:
		'Error message in the channel and chat textual attachment code panel. Preserve {previewLimitKb}; it is inserted by code.',
});
const UNABLE_TO_LOAD_PREVIEW_DESCRIPTOR = msg({
	message: 'Unable to load preview.',
	comment: 'Error message in the channel and chat textual attachment code panel.',
});

export function TextualAttachmentCodePanel({
	canExpand,
	copyTextContent,
	fillAvailableSpace = false,
	highlightedHtml,
	isExpanded,
	previewError,
	status,
	textContent,
	visibleLineCount,
	wrapText,
	wrapperClassName,
}: TextualAttachmentCodePanelProps) {
	const {i18n} = useLingui();
	const hasLoadedText = textContent !== null;
	const panelStyle = useMemo<PreviewPanelStyle>(
		() => ({
			'--preview-visible-lines': `${Math.max(visibleLineCount, 1)}`,
		}),
		[visibleLineCount],
	);
	if (status === 'error') {
		return (
			<div
				className={clsx(styles.previewSurface, wrapperClassName)}
				style={panelStyle}
				aria-live="polite"
				data-flx="channel.embeds.attachments.textual-attachment-code-panel.preview-surface"
			>
				<div
					className={styles.previewError}
					data-flx="channel.embeds.attachments.textual-attachment-code-panel.preview-error"
				>
					<WarningCircleIcon
						size={16}
						weight="bold"
						data-flx="channel.embeds.attachments.textual-attachment-code-panel.warning-circle-icon"
					/>
					<span data-flx="channel.embeds.attachments.textual-attachment-code-panel.span">
						{`${
							previewError?.type === 'size'
								? i18n._(FILE_IS_TOO_LARGE_FOR_INLINE_PREVIEW_LIMIT_DESCRIPTOR, {previewLimitKb: PREVIEW_LIMIT_KB})
								: i18n._(UNABLE_TO_LOAD_PREVIEW_DESCRIPTOR)
						}${previewError?.type === 'network' && previewError.message ? ` ${previewError.message}` : ''}`}
					</span>
				</div>
			</div>
		);
	}
	if (!hasLoadedText) {
		return (
			<div
				className={clsx(styles.previewSurface, wrapperClassName)}
				style={panelStyle}
				aria-live="polite"
				data-flx="channel.embeds.attachments.textual-attachment-code-panel.preview-surface--2"
			>
				<div
					className={styles.loadingState}
					data-flx="channel.embeds.attachments.textual-attachment-code-panel.loading-state"
				>
					<Spinner size="small" data-flx="channel.embeds.attachments.textual-attachment-code-panel.spinner" />
				</div>
			</div>
		);
	}
	return (
		<div
			className={clsx(styles.previewSurface, wrapperClassName)}
			style={panelStyle}
			aria-live="polite"
			data-flx="channel.embeds.attachments.textual-attachment-code-panel.preview-surface--3"
		>
			<CopyButton
				value={copyTextContent ?? textContent ?? ''}
				label={COPY_TEXT_DESCRIPTOR}
				className={styles.previewActions}
				visibleClassName={styles.previewActionsVisible}
				buttonClassName={styles.previewActionButton}
				iconClassName={styles.copyIcon}
				data-flx="channel.embeds.attachments.textual-attachment-code-panel.preview-actions"
			/>
			<div
				className={clsx(
					styles.previewViewport,
					(isExpanded && canExpand) || fillAvailableSpace
						? styles.previewViewportExpanded
						: styles.previewViewportCollapsed,
					fillAvailableSpace && styles.previewViewportFill,
				)}
				data-flx="channel.embeds.attachments.textual-attachment-code-panel.preview-viewport"
			>
				<pre
					className={clsx(
						styles.previewCode,
						wrapText && styles.previewCodeWrap,
						fillAvailableSpace && styles.previewCodeFill,
					)}
					data-flx="channel.embeds.attachments.textual-attachment-code-panel.preview-code"
				>
					<code
						className="hljs"
						dangerouslySetInnerHTML={{__html: highlightedHtml}}
						data-flx="channel.embeds.attachments.textual-attachment-code-panel.hljs"
					/>
				</pre>
			</div>
		</div>
	);
}
