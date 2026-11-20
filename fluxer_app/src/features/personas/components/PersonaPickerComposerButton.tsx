import Accessibility from '@app/features/accessibility/state/Accessibility';
import buttonStyles from '@app/features/channel/components/textarea/TextareaButton.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import {Button} from '@app/features/ui/button/Button';
import {Avatar} from '@app/features/ui/components/Avatar';
import {openPopout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import Personas from '@app/features/user/state/Personas';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react';
import {UserSwitchIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {runInAction} from 'mobx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';
import * as PersonaCommands from '../commands/Personas';
import PersonaPickerMobile from '../state/PersonaPickerMobile';
import styles from './PersonaPickerComposerButton.module.css';
import {PersonaPickerPopout} from './popouts/PersonaPickerPopout';

const SELECT_PERSONA_DESCRIPTOR = msg({
	message: 'Select persona',
	comment: 'Tooltip for the persona selection button',
});

interface PersonaPickerComposerButtonProps {
	channel: Channel;
}

export const PersonaPickerComposerButton = observer<PersonaPickerComposerButtonProps>((props) => {
	const i18n = useLingui();
	const personaPickerRef = useRef<HTMLButtonElement | null>(null);
	const [personaPickerOpen, setPersonaPickerOpen] = useState(false);
	const getActivePersona = useCallback(() => Personas.getGlobalActivePersona(), []);
	const [hasPersonas, setHasPersonas] = useState(() => !!Personas.getOwnPersonas().length);
	const [selectedPersona, setSelectedPersona] = useState(getActivePersona);
	const isMobile = MobileLayout.isEnabled();

	useEffect(() => {
		void PersonaCommands.fetchUserPersonas().then((personas) => {
			runInAction(() => {
				Personas.cachePersonas(personas);
			});
			setHasPersonas(!!Personas.getOwnPersonas().length);
		});

		return Personas.subscribe(() => {
			setSelectedPersona(getActivePersona);
			setHasPersonas(!!Personas.getOwnPersonas().length);
		});
	}, []);

	return (
		hasPersonas &&
		Accessibility.showPersonaPickerButton && (
			<Tooltip text={i18n._(SELECT_PERSONA_DESCRIPTOR)} data-flx="personas.persona-picker-composer-button.tooltip">
				<Button
					ref={personaPickerRef}
					aria-label="Select persona"
					square
					compact
					variant="ghost"
					onClick={(_: React.MouseEvent) => {
						if (isMobile) {
							PersonaPickerMobile.open({
								channel: props.channel,
								onSelect: (p) => Personas.setGlobalActivePersona(p || ''),
							});
						} else {
							openPopout(
								personaPickerRef.current!,
								{
									position: 'top-start',
									render: () => (
										<PersonaPickerPopout
											channel={props.channel}
											onSelect={(p) => Personas.setGlobalActivePersona(p || '')}
											data-flx="personas.persona-picker-composer-button.persona-picker-popout.set-global-active-persona"
										/>
									),
									shouldAutoUpdate: false,
									onOpen: () => setPersonaPickerOpen(true),
									onClose: () => setPersonaPickerOpen(false),
								},
								0,
							);
						}
					}}
					className={clsx(
						buttonStyles.button,
						buttonStyles.buttonCompact,
						styles.personaAvatar,
						styles.buttonMarker,
						personaPickerOpen && styles.contextMenuHover,
					)}
					icon={
						<div
							className={clsx(styles.personaAvatar, personaPickerOpen && styles.contextMenuHover)}
							data-flx="personas.persona-picker-composer-button.persona-avatar--2"
						>
							{Personas.getGlobalActivePersona() ? (
								<Avatar
									size={36}
									user={Users.currentUser!}
									personaId={selectedPersona?.id}
									personaAvatar={selectedPersona?.avatar || undefined}
									data-flx="personas.persona-picker-composer-button.avatar"
								/>
							) : (
								<UserSwitchIcon
									className={buttonStyles.icon}
									data-flx="personas.persona-picker-composer-button.user-switch-icon"
								/>
							)}
						</div>
					}
					data-flx="personas.persona-picker-composer-button.persona-avatar"
				/>
			</Tooltip>
		)
	);
});
