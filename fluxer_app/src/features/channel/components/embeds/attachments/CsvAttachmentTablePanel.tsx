// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildCsvTableNode,
	type CsvRows,
} from '@app/features/channel/components/embeds/attachments/CsvAttachmentPreviewUtils';
import styles from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreview.module.css';
import {
	PREVIEW_LIMIT_KB,
	type PreviewError,
	type PreviewPanelStyle,
	type PreviewStatus,
} from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreviewUtils';
import {COPY_TEXT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {render} from '@app/features/messaging/components/markdown/renderers';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
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

interface CsvAttachmentTablePanelProps {
	canExpand: boolean;
	copyTextContent?: string | null;
	fillAvailableSpace?: boolean;
	isExpanded: boolean;
	previewError: PreviewError | null;
	rows: CsvRows | null;
	status: PreviewStatus;
	visibleLineCount: number;
	wrapperClassName?: string;
}

export function CsvAttachmentTablePanel({
	canExpand,
	copyTextContent,
	fillAvailableSpace = false,
	isExpanded,
	previewError,
	rows,
	status,
	visibleLineCount,
	wrapperClassName,
}: CsvAttachmentTablePanelProps) {
	const {i18n} = useLingui();
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
				data-flx="channel.embeds.attachments.csv-attachment-table-panel.preview-surface"
			>
				<div
					className={styles.previewError}
					data-flx="channel.embeds.attachments.csv-attachment-table-panel.preview-error"
				>
					<WarningCircleIcon
						size={16}
						weight="bold"
						data-flx="channel.embeds.attachments.csv-attachment-table-panel.warning-circle-icon"
					/>
					<span data-flx="channel.embeds.attachments.csv-attachment-table-panel.span">
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
	if (rows === null) {
		return (
			<div
				className={clsx(styles.previewSurface, wrapperClassName)}
				style={panelStyle}
				aria-live="polite"
				data-flx="channel.embeds.attachments.csv-attachment-table-panel.preview-surface--2"
			>
				<div
					className={styles.loadingState}
					data-flx="channel.embeds.attachments.csv-attachment-table-panel.loading-state"
				>
					<Spinner size="small" data-flx="channel.embeds.attachments.csv-attachment-table-panel.spinner" />
				</div>
			</div>
		);
	}
	const tableNode = buildCsvTableNode(rows);
	const renderedTable = render([tableNode], {
		context: MarkdownContext.STANDARD_WITHOUT_JUMBO,
		disableInteractions: true,
	});
	return (
		<div
			className={clsx(styles.previewSurface, wrapperClassName)}
			style={panelStyle}
			aria-live="polite"
			data-flx="channel.embeds.attachments.csv-attachment-table-panel.preview-surface--3"
		>
			<CopyButton
				value={copyTextContent ?? ''}
				label={COPY_TEXT_DESCRIPTOR}
				className={styles.previewActions}
				visibleClassName={styles.previewActionsVisible}
				buttonClassName={styles.previewActionButton}
				iconClassName={styles.copyIcon}
				data-flx="channel.embeds.attachments.csv-attachment-table-panel.preview-actions"
			/>
			<div
				className={clsx(
					styles.csvTableViewport,
					(isExpanded && canExpand) || fillAvailableSpace
						? styles.previewViewportExpanded
						: styles.previewViewportCollapsed,
					fillAvailableSpace && styles.previewViewportFill,
				)}
				data-flx="channel.embeds.attachments.csv-attachment-table-panel.csv-table-viewport"
			>
				<div
					className={clsx(markupStyles.markup, styles.csvTableMarkup)}
					data-flx="channel.embeds.attachments.csv-attachment-table-panel.csv-table-markup"
				>
					{renderedTable}
				</div>
			</div>
		</div>
	);
}
