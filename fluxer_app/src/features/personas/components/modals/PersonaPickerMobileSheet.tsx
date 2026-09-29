import { observer } from "mobx-react-lite";
import PersonaPickerMobile from "../../state/PersonaPickerMobile";
import { PersonaPickerPopout } from "../popouts/PersonaPickerPopout";
import { BottomSheet } from "@app/features/ui/bottom_sheet/BottomSheet";

export const PersonaPickerMobileSheet = observer(() => {
	const state = PersonaPickerMobile;

	return <BottomSheet isOpen={state.isOpen()} onClose={state.close}>
		{state.isOpen() && <PersonaPickerPopout
			key={`${state.channel?.id}_m${state.messageId}`}
			channel={state.channel!}
			onSelect={state.onSelect!}
			selectedId={state.selectedId}
			isMobile={true}
		/>}
	</BottomSheet>
});
