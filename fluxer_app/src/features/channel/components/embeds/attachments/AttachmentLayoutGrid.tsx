// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AttachmentGridItem,
	type LayoutType,
} from '@app/features/channel/components/embeds/attachments/AttachmentGridItem';
import {getMosaicTileBox} from '@app/features/channel/components/MessageAttachmentUtils';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getMosaicMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import styles from '@app/features/theme/styles/AttachmentLayoutGrid.module.css';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {observer} from 'mobx-react-lite';
import type {FC} from 'react';

export interface AttachmentLayoutGridProps {
	attachments: ReadonlyArray<MessageAttachment>;
	message?: Message;
	isPreview?: boolean;
	snapshotIndex?: number;
}

interface LayoutConfig {
	type: LayoutType;
	gridClassName: string;
}

const LAYOUT_CONFIGS: Record<number, LayoutConfig> = {
	2: {type: 'two', gridClassName: styles.twoImageGrid},
	3: {type: 'three', gridClassName: styles.threeImageGrid},
	4: {type: 'four', gridClassName: styles.fourImageGrid},
	5: {type: 'five', gridClassName: styles.fiveImageGrid},
	6: {type: 'six', gridClassName: styles.sixImageGrid},
	7: {type: 'seven', gridClassName: styles.sevenImageContainer},
	8: {type: 'eight', gridClassName: styles.eightImageContainer},
	9: {type: 'nine', gridClassName: styles.nineImageGrid},
	10: {type: 'ten', gridClassName: styles.tenImageContainer},
};

function getLayoutConfig(count: number): LayoutConfig {
	return LAYOUT_CONFIGS[count] || LAYOUT_CONFIGS[4];
}

export const AttachmentLayoutGrid: FC<AttachmentLayoutGridProps> = observer(
	({attachments, message, isPreview, snapshotIndex}) => {
		const count = attachments.length;
		const config = getLayoutConfig(count);
		const mosaicWidth = getMosaicMediaDimensions().maxWidth;
		const tileAspectRatio = (index: number) => getMosaicTileBox(count, index, mosaicWidth).aspectRatio;
		if (count === 7) {
			return (
				<div
					className={styles.sevenImageContainer}
					data-flx="channel.embeds.attachments.attachment-layout-grid.seven-image-container"
				>
					<div className={styles.sevenHero} data-flx="channel.embeds.attachments.attachment-layout-grid.seven-hero">
						<AttachmentGridItem
							key={attachments[0].id}
							attachment={attachments[0]}
							targetAspectRatio={tileAspectRatio(0)}
							message={message}
							mediaAttachments={attachments}
							isPreview={isPreview}
							snapshotIndex={snapshotIndex}
							data-flx="channel.embeds.attachments.attachment-layout-grid.attachment-grid-item"
						/>
					</div>
					<div className={styles.sevenGrid} data-flx="channel.embeds.attachments.attachment-layout-grid.seven-grid">
						{attachments.slice(1, 7).map((attachment, index) => (
							<AttachmentGridItem
								key={attachment.id}
								attachment={attachment}
								targetAspectRatio={tileAspectRatio(index + 1)}
								message={message}
								mediaAttachments={attachments}
								isPreview={isPreview}
								snapshotIndex={snapshotIndex}
								data-flx="channel.embeds.attachments.attachment-layout-grid.attachment-grid-item--2"
							/>
						))}
					</div>
				</div>
			);
		}
		if (count === 8) {
			return (
				<div
					className={styles.eightImageContainer}
					data-flx="channel.embeds.attachments.attachment-layout-grid.eight-image-container"
				>
					<div
						className={styles.eightTopRow}
						data-flx="channel.embeds.attachments.attachment-layout-grid.eight-top-row"
					>
						{attachments.slice(0, 2).map((attachment, index) => (
							<AttachmentGridItem
								key={attachment.id}
								attachment={attachment}
								targetAspectRatio={tileAspectRatio(index)}
								message={message}
								mediaAttachments={attachments}
								isPreview={isPreview}
								snapshotIndex={snapshotIndex}
								data-flx="channel.embeds.attachments.attachment-layout-grid.attachment-grid-item--3"
							/>
						))}
					</div>
					<div
						className={styles.eightBottomGrid}
						data-flx="channel.embeds.attachments.attachment-layout-grid.eight-bottom-grid"
					>
						{attachments.slice(2, 8).map((attachment, index) => (
							<AttachmentGridItem
								key={attachment.id}
								attachment={attachment}
								targetAspectRatio={tileAspectRatio(index + 2)}
								message={message}
								mediaAttachments={attachments}
								isPreview={isPreview}
								snapshotIndex={snapshotIndex}
								data-flx="channel.embeds.attachments.attachment-layout-grid.attachment-grid-item--4"
							/>
						))}
					</div>
				</div>
			);
		}
		if (count === 10) {
			return (
				<div
					className={styles.tenImageContainer}
					data-flx="channel.embeds.attachments.attachment-layout-grid.ten-image-container"
				>
					<div className={styles.tenHero} data-flx="channel.embeds.attachments.attachment-layout-grid.ten-hero">
						<AttachmentGridItem
							key={attachments[0].id}
							attachment={attachments[0]}
							targetAspectRatio={tileAspectRatio(0)}
							message={message}
							mediaAttachments={attachments}
							isPreview={isPreview}
							snapshotIndex={snapshotIndex}
							data-flx="channel.embeds.attachments.attachment-layout-grid.attachment-grid-item--5"
						/>
					</div>
					<div className={styles.tenGrid} data-flx="channel.embeds.attachments.attachment-layout-grid.ten-grid">
						{attachments.slice(1, 10).map((attachment, index) => (
							<AttachmentGridItem
								key={attachment.id}
								attachment={attachment}
								targetAspectRatio={tileAspectRatio(index + 1)}
								message={message}
								mediaAttachments={attachments}
								isPreview={isPreview}
								snapshotIndex={snapshotIndex}
								data-flx="channel.embeds.attachments.attachment-layout-grid.attachment-grid-item--6"
							/>
						))}
					</div>
				</div>
			);
		}
		return (
			<div className={config.gridClassName} data-flx="channel.embeds.attachments.attachment-layout-grid.div">
				{attachments.map((attachment, index) => (
					<AttachmentGridItem
						key={attachment.id}
						attachment={attachment}
						targetAspectRatio={tileAspectRatio(index)}
						message={message}
						mediaAttachments={attachments}
						isPreview={isPreview}
						snapshotIndex={snapshotIndex}
						data-flx="channel.embeds.attachments.attachment-layout-grid.attachment-grid-item--7"
					/>
				))}
			</div>
		);
	},
);
