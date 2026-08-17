// SPDX-License-Identifier: AGPL-3.0-or-later

import {useElementOverflow} from '@app/features/app/hooks/useTextOverflow';
import {Message} from '@app/features/channel/components/ChannelMessage';
import type {Channel} from '@app/features/channel/models/Channel';
import styles from '@app/features/moderation/components/report_modal/IARModal.module.css';
import type {IARContext} from '@app/features/moderation/components/report_modal/IARModalTypes';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';
import {useState} from 'react';

const COMMUNITY_DESCRIPTOR = msg({
	message: 'Community',
	comment:
		'Developer / debug surface — keep terse and technical. Fallback label used in the IAR preview tool when no community context is supplied.',
});

interface IARModalPreviewProps {
	context: IARContext;
	currentChannel: Channel | null;
}

export const IARModalPreview: React.FC<IARModalPreviewProps> = ({context, currentChannel}) => {
	const {i18n} = useLingui();
	const [previewElement, setPreviewElement] = useState<HTMLDivElement | null>(null);
	const isPreviewOverflowing = useElementOverflow(previewElement, 'vertical');
	switch (context.type) {
		case 'message':
			if (currentChannel === null) {
				return null;
			}
			return (
				<div
					ref={setPreviewElement}
					className={clsx(styles.preview, isPreviewOverflowing && styles.previewOverflowing)}
					data-flx="moderation.iar-modal-preview.preview"
				>
					<Message
						channel={currentChannel}
						message={context.message}
						previewContext={MessagePreviewContext.LIST_POPOUT}
						removeTopSpacing={true}
						data-flx="moderation.iar-modal-preview.message"
					/>
				</div>
			);
		case 'user':
			return (
				<div className={styles.identityPreview} data-flx="moderation.iar-modal-preview.identity-preview">
					<div className={styles.previewTitle} data-flx="moderation.iar-modal-preview.preview-title">
						{NicknameUtils.getDisplayName(context.user)}
					</div>
					<div className={styles.previewSubtitle} data-flx="moderation.iar-modal-preview.preview-subtitle">
						{NicknameUtils.formatUserTagForStreamerMode(context.user)}
					</div>
				</div>
			);
		case 'guild':
			return (
				<div className={styles.identityPreview} data-flx="moderation.iar-modal-preview.identity-preview--2">
					<div className={styles.previewTitle} data-flx="moderation.iar-modal-preview.preview-title--2">
						{context.guild.name}
					</div>
					<div className={styles.previewSubtitle} data-flx="moderation.iar-modal-preview.preview-subtitle--2">
						{i18n._(COMMUNITY_DESCRIPTOR)}
					</div>
				</div>
			);
	}
};
