import type { Channel } from "@app/features/channel/models/Channel";
import { Logger } from "@app/features/platform/utils/AppLogger";
import { makeAutoObservable } from "mobx";
import type { PersonaPickerPopoutProps } from "../components/popouts/PersonaPickerPopout";

interface PersonaPickerOpenProps {
	messageId?: string;
	onClose?: () => void;
}

class PersonaPickerMobile {
	private logger = new Logger('PersonaProfileMobile');
	channel: PersonaPickerPopoutProps['channel'] | undefined = undefined;
	selectedId: PersonaPickerPopoutProps['selectedId'] | undefined = undefined;
	onSelect: PersonaPickerPopoutProps['onSelect'] | undefined = undefined;
	messageId: PersonaPickerOpenProps['messageId'] | undefined = undefined;
	onClose: PersonaPickerOpenProps['onClose'] | undefined = undefined;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	isOpen(): boolean {
		return !!this.channel && !!this.onSelect;
	}

	open({channel, selectedId, onSelect, messageId, onClose} : Omit<PersonaPickerPopoutProps, 'isMobile'> & PersonaPickerOpenProps) {
		this.channel = channel;
		this.selectedId = selectedId;
		this.onSelect = onSelect;
		this.messageId = messageId;
		this.onClose = onClose;
	}

	close() {
		this.onClose?.();
		this.channel = undefined;
		this.selectedId = undefined;
		this.onSelect = undefined;
		this.messageId = undefined;
		this.onClose = undefined;
	}
}

export default new PersonaPickerMobile();
