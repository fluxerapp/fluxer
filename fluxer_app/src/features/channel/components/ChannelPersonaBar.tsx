// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelEditBar.module.css';
import wrapperStyles from '@app/features/channel/components/textarea/InputWrapper.module.css';
import type {Persona} from '@app/features/personas/models/Persona';
import {Avatar} from '@app/features/ui/components/Avatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useRef} from 'react';

const CANCEL_PERSONA_DESCRIPTOR = msg({
	message: "Don't use this persona",
	comment: 'Button or menu action label in the channel and chat persona bar. Keep it concise.',
});

interface PersonaBarProps {
	persona: Persona;
	onCancel: () => void;
}

export const PersonaBar = observer(({persona, onCancel}: PersonaBarProps) => {
	const {i18n} = useLingui();
	const user = useRef(Users.currentUser);
	const handlePersonaCancel = () => {
		onCancel();
	};
	return (
		<div
			className={`${wrapperStyles.box} ${wrapperStyles.wrapperSides} ${wrapperStyles.roundedTop} ${wrapperStyles.noBottomBorder}`}
			data-flx="channel.edit-bar.div"
		>
			<div
				className={wrapperStyles.barInner}
				style={{gridTemplateColumns: '1fr auto'}}
				data-flx="channel.edit-bar.div--2"
			>
				<div className={styles.text} data-flx="channel.edit-bar.text">
					<Trans>
						Sending this message as{' '}
						<Avatar
							forceAnimate={true}
							className={styles.personaAvatar}
							user={user.current!}
							size={16}
							personaAvatar={persona.avatar}
							personaId={persona.id}
							data-flx="channel.channel-persona-bar.persona-bar.persona-avatar"
						/>{' '}
						<span className={styles.personaName} data-flx="channel.channel-persona-bar.persona-bar.persona-name">
							{persona.display_name || persona.internal_name}
						</span>
					</Trans>
				</div>
				<div className={styles.controls} data-flx="channel.edit-bar.controls">
					<FocusRing offset={-2} data-flx="channel.edit-bar.focus-ring">
						<button
							type="button"
							className={styles.button}
							onClick={handlePersonaCancel}
							aria-label={i18n._(CANCEL_PERSONA_DESCRIPTOR)}
							data-flx="channel.edit-bar.button.stop-edit"
						>
							<XCircleIcon className={styles.icon} data-flx="channel.edit-bar.icon" />
						</button>
					</FocusRing>
				</div>
			</div>
			<div className={wrapperStyles.separator} data-flx="channel.edit-bar.div--3" />
		</div>
	);
});
