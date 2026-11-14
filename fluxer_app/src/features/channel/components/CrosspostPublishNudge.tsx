// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/CrosspostPublishNudge.module.css';
import {requestMessageCrosspost, useMessagePermissions} from '@app/features/channel/components/MessageActionUtils';
import type {Channel} from '@app/features/channel/models/Channel';
import PublishNudge from '@app/features/channel/state/PublishNudge';
import {DISMISS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {AnnouncementChannelIcon} from '@app/features/ui/components/icons/AnnouncementChannelIcon';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const SHARE_WITH_FOLLOWERS_DESCRIPTOR = msg({
	message: 'Not sent to followers yet.',
	comment:
		'Prompt under the newest message the current user sent in an announcement channel, suggesting they publish it to the communities that follow the channel.',
});
const PUBLISH_DESCRIPTOR = msg({
	message: 'Publish',
	comment:
		'Link-style button in the prompt under a new announcement channel message. Publishes the message to the communities that follow the channel.',
});
const DONT_SHOW_AGAIN_DESCRIPTOR = msg({
	message: "Don't show again",
	comment:
		'Muted button next to the publish prompt under a new announcement channel message. Hides the prompt for every future message.',
});

interface CrosspostPublishNudgeProps {
	message: Message;
	channel: Channel;
}

export const CrosspostPublishNudge = observer(({message, channel}: CrosspostPublishNudgeProps) => {
	const {i18n} = useLingui();
	const permissions = useMessagePermissions(message, channel);
	if (!PublishNudge.shouldShow(message, permissions?.canCrosspostMessage ?? false)) {
		return null;
	}
	const handlePublish = (event: React.MouseEvent) => {
		requestMessageCrosspost(message, i18n, {shiftKey: event.shiftKey});
	};
	return (
		<div className={styles.row} data-message-copy-hidden="true" data-flx="channel.crosspost-publish-nudge.row">
			<div className={styles.pill} data-flx="channel.crosspost-publish-nudge.pill">
				<AnnouncementChannelIcon className={styles.icon} data-flx="channel.crosspost-publish-nudge.icon" />
				<span className={styles.label} data-flx="channel.crosspost-publish-nudge.label">
					{i18n._(SHARE_WITH_FOLLOWERS_DESCRIPTOR)}
				</span>
				<FocusRing data-flx="channel.crosspost-publish-nudge.publish-focus-ring">
					<button
						type="button"
						className={styles.publishButton}
						onClick={handlePublish}
						data-flx="channel.crosspost-publish-nudge.publish-button.publish"
					>
						{i18n._(PUBLISH_DESCRIPTOR)}
					</button>
				</FocusRing>
				<FocusRing data-flx="channel.crosspost-publish-nudge.dismiss-focus-ring">
					<button
						type="button"
						className={styles.dismissButton}
						aria-label={i18n._(DISMISS_DESCRIPTOR)}
						onClick={() => PublishNudge.dismiss(message.id)}
						data-flx="channel.crosspost-publish-nudge.dismiss-button.dismiss"
					>
						<XCircleIcon
							weight="fill"
							className={styles.dismissIcon}
							aria-hidden="true"
							data-flx="channel.crosspost-publish-nudge.dismiss-icon"
						/>
					</button>
				</FocusRing>
			</div>
			<FocusRing data-flx="channel.crosspost-publish-nudge.hide-forever-focus-ring">
				<button
					type="button"
					className={styles.hideForeverButton}
					onClick={PublishNudge.hideForever}
					data-flx="channel.crosspost-publish-nudge.hide-forever-button.hide-forever"
				>
					{i18n._(DONT_SHOW_AGAIN_DESCRIPTOR)}
				</button>
			</FocusRing>
		</div>
	);
});
