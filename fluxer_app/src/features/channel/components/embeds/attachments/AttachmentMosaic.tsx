// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ExpiryFootnote} from '@app/features/app/components/shared/ExpiryFootnote';
import {AttachmentLayoutGrid} from '@app/features/channel/components/embeds/attachments/AttachmentLayoutGrid';
import {AttachmentSingleMedia} from '@app/features/channel/components/embeds/attachments/AttachmentSingleMedia';
import {isMediaAttachment} from '@app/features/channel/components/MessageAttachmentUtils';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {formatAttachmentDate, getEarliestAttachmentExpiry} from '@app/features/messaging/utils/AttachmentExpiryUtils';
import {getMosaicMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme/styles/AttachmentMosaic.module.css';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {CSSProperties, FC} from 'react';
import {useMemo} from 'react';

const EXPIRED_BETWEEN_AND_DESCRIPTOR = msg({
	message: 'Expired between {earliest} and {latest}',
	comment:
		'Error message in the channel and chat attachment mosaic. Preserve {earliest}, {latest}; they are inserted by code.',
});
const EXPIRES_BETWEEN_AND_DESCRIPTOR = msg({
	message: 'Expires between {earliest} and {latest}',
	comment: 'Label in the channel and chat attachment mosaic. Preserve {earliest}, {latest}; they are inserted by code.',
});
const EXPIRED_ON_DESCRIPTOR = msg({
	message: 'Expired on {earliest}',
	comment: 'Error message in the channel and chat attachment mosaic. Preserve {earliest}; it is inserted by code.',
});
const EXPIRES_ON_DESCRIPTOR = msg({
	message: 'Expires on {earliest}',
	comment:
		'Short label in the channel and chat attachment mosaic. Keep it concise. Preserve {earliest}; it is inserted by code.',
});

export interface AttachmentMosaicProps {
	attachments: ReadonlyArray<MessageAttachment>;
	message?: Message;
	hideExpiryFootnote?: boolean;
	isPreview?: boolean;
	snapshotIndex?: number;
	onDelete?: (bypassConfirm?: boolean) => void;
}

interface AttachmentMosaicStyle extends CSSProperties {
	'--attachment-media-max-height': string;
	'--attachment-media-max-width': string;
}

const AttachmentMosaicComponent: FC<AttachmentMosaicProps> = observer(
	({attachments, message, hideExpiryFootnote, isPreview, snapshotIndex, onDelete}) => {
		const {i18n} = useLingui();
		const mediaAttachments = useMemo(() => attachments.filter(isMediaAttachment), [attachments]);
		const mosaicDimensions = getMosaicMediaDimensions();
		const mosaicStyle: AttachmentMosaicStyle = {
			'--attachment-media-max-height': remFromPx(mosaicDimensions.maxHeight),
			'--attachment-media-max-width': remFromPx(mosaicDimensions.maxWidth),
		};
		if (mediaAttachments.length === 0) {
			return null;
		}
		const aggregateExpiry = getEarliestAttachmentExpiry(attachments);
		const renderFootnote = () => {
			if (hideExpiryFootnote) {
				return null;
			}
			const earliest = formatAttachmentDate(aggregateExpiry.expiresAt);
			const latest = formatAttachmentDate(aggregateExpiry.latestAt);
			let label: string;
			if (earliest && latest && earliest !== latest) {
				label = aggregateExpiry.isExpired
					? i18n._(EXPIRED_BETWEEN_AND_DESCRIPTOR, {earliest, latest})
					: i18n._(EXPIRES_BETWEEN_AND_DESCRIPTOR, {earliest, latest});
			} else if (earliest) {
				label = aggregateExpiry.isExpired
					? i18n._(EXPIRED_ON_DESCRIPTOR, {earliest})
					: i18n._(EXPIRES_ON_DESCRIPTOR, {earliest});
			} else {
				return null;
			}
			return Accessibility.showAttachmentExpiryIndicator ? (
				<ExpiryFootnote
					expiresAt={aggregateExpiry.expiresAt}
					isExpired={aggregateExpiry.isExpired}
					label={label}
					data-flx="channel.embeds.attachments.attachment-mosaic.render-footnote.expiry-footnote"
				/>
			) : null;
		};
		return (
			<div
				className={styles.mosaicContainerWrapper}
				style={mosaicStyle}
				data-flx="channel.embeds.attachments.attachment-mosaic.attachment-mosaic-component.mosaic-container-wrapper"
			>
				<div
					className={styles.mosaicContainer}
					data-flx="channel.embeds.attachments.attachment-mosaic.attachment-mosaic-component.mosaic-container"
				>
					{mediaAttachments.length === 1 ? (
						<AttachmentSingleMedia
							attachment={mediaAttachments[0]}
							message={message}
							mediaAttachments={mediaAttachments}
							isPreview={isPreview}
							snapshotIndex={snapshotIndex}
							onDelete={onDelete}
							data-flx="channel.embeds.attachments.attachment-mosaic.attachment-mosaic-component.attachment-single-media"
						/>
					) : (
						<AttachmentLayoutGrid
							attachments={mediaAttachments}
							message={message}
							isPreview={isPreview}
							snapshotIndex={snapshotIndex}
							data-flx="channel.embeds.attachments.attachment-mosaic.attachment-mosaic-component.attachment-layout-grid"
						/>
					)}
				</div>
				{renderFootnote()}
			</div>
		);
	},
);
export const AttachmentMosaic: FC<AttachmentMosaicProps> = AttachmentMosaicComponent;
