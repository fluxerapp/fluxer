import { Button, type ButtonProps } from "@app/features/ui/button/Button";
import { openPopout } from "@app/features/ui/popover/PopoverPopout";
import { observer } from "mobx-react-lite";
import { PersonaPickerPopout } from "./popouts/PersonaPickerPopout";
import { useActionState, useCallback, useEffect, useRef, useState, type RefAttributes } from "react";
import type { Channel } from "@app/features/channel/models/Channel";
import Personas from "@app/features/user/state/Personas";
import { clsx } from "clsx";
import buttonStyles from '@app/features/channel/components/textarea/TextareaButton.module.css';
import styles from './PersonaPickerComposerButton.module.css';
import { Avatar } from "@app/features/ui/components/Avatar";
import Users from "@app/features/user/state/Users";
import { UserSwitchIcon } from "@phosphor-icons/react";
import { Tooltip } from "@app/features/ui/tooltip/Tooltip";
import { useLingui } from "@lingui/react";
import { msg } from "@lingui/core/macro";
import UserSettings from "@app/features/user/state/UserSettings";
import * as PersonaCommands from "../commands/Personas";
import { runInAction } from "mobx";
import MobileLayout from "@app/features/ui/state/MobileLayout";
import PersonaPickerMobile from "../state/PersonaPickerMobile";
import Accessibility from "@app/features/accessibility/state/Accessibility";

const SELECT_PERSONA_DESCRIPTOR = msg({
	message: 'Select persona',
	comment: 'Tooltip for the persona selection button'
});

interface PersonaPickerComposerButtonProps {
	channel: Channel,
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
		})
	}, []);

	return hasPersonas && Accessibility.showPersonaPickerButton && <Tooltip text={i18n._(SELECT_PERSONA_DESCRIPTOR)}>
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
						onSelect: (p) => Personas.setGlobalActivePersona(p || ""),
					});
				} else {
					openPopout(personaPickerRef.current!, {
						position: "top-start",
						render: () => <PersonaPickerPopout
							channel={props.channel}
							onSelect={(p) => Personas.setGlobalActivePersona(p || "")}
						/>,
						shouldAutoUpdate: false,
						onOpen: () => setPersonaPickerOpen(true),
						onClose: () => setPersonaPickerOpen(false),
					}, 0);
				}
			}}
			className={clsx(buttonStyles.button, buttonStyles.buttonCompact, styles.personaAvatar, styles.buttonMarker, personaPickerOpen && styles.contextMenuHover)}
			icon={<div className={clsx(styles.personaAvatar, personaPickerOpen && styles.contextMenuHover)}>{
				Personas.getGlobalActivePersona() ? <Avatar
					size={36}
					user={Users.currentUser!}
					personaId={selectedPersona?.id}
					personaAvatar={selectedPersona?.avatar || undefined}
				/> : <UserSwitchIcon className={buttonStyles.icon} />
			}</div>}
		/>
	</Tooltip>;
})
