// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {joinVisitedGuild, leaveVisit} from '@app/features/guild/commands/GuildVisitCommands';
import type {Guild} from '@app/features/guild/models/Guild';
import {isolateBidi} from '@app/features/i18n/utils/BidiIsolate';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const PREVIEWING_DESCRIPTOR = msg({
	message: "You're previewing {community}.",
	comment:
		'Bar at the top of a community the person is reading without having joined. community is its name. Previewing means reading before joining.',
});
export const JOIN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Join',
	comment: 'Button that joins the community the person is previewing.',
});
export const STOP_PREVIEWING_DESCRIPTOR = msg({
	message: 'Stop previewing',
	comment: 'Button or menu item that ends the preview of a community the person has not joined and leaves it.',
});

const tone = NAGBAR_TONES[NagbarToneKind.BRAND];

export const GuildPreviewNagbar = observer(function GuildPreviewNagbar({
	guild,
	isMobile,
}: {
	guild: Guild;
	isMobile: boolean;
}) {
	const {i18n} = useLingui();
	const [pending, setPending] = useState<'join' | 'stop' | null>(null);
	const run = async (action: 'join' | 'stop') => {
		if (pending !== null) return;
		setPending(action);
		try {
			await (action === 'join' ? joinVisitedGuild(guild.id) : leaveVisit(guild.id));
		} finally {
			setPending(null);
		}
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={tone.backgroundColor}
			textColor={tone.textColor}
			data-flx="guild.visits.preview-nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={i18n._(PREVIEWING_DESCRIPTOR, {community: isolateBidi(guild.name)})}
				actions={
					<>
						<NagbarButton
							isMobile={isMobile}
							variant="inverted-outline"
							onClick={() => void run('stop')}
							submitting={pending === 'stop'}
							disabled={pending === 'join'}
							data-flx="guild.visits.preview-nagbar.button.stop"
						>
							{i18n._(STOP_PREVIEWING_DESCRIPTOR)}
						</NagbarButton>
						<NagbarButton
							isMobile={isMobile}
							onClick={() => void run('join')}
							submitting={pending === 'join'}
							disabled={pending === 'stop'}
							data-flx="guild.visits.preview-nagbar.button.join"
						>
							{i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
						</NagbarButton>
					</>
				}
				data-flx="guild.visits.preview-nagbar.content"
			/>
		</Nagbar>
	);
});
