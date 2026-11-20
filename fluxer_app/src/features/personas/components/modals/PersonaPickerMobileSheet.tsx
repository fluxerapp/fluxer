import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {observer} from 'mobx-react-lite';
import PersonaPickerMobile from '../../state/PersonaPickerMobile';
import {PersonaPickerPopout} from '../popouts/PersonaPickerPopout';

export const PersonaPickerMobileSheet = observer(() => {
	const state = PersonaPickerMobile;

	return (
		<BottomSheet
			isOpen={state.isOpen()}
			onClose={state.close}
			data-flx="personas.persona-picker-mobile-sheet.bottom-sheet"
		>
			{state.isOpen() && (
				<PersonaPickerPopout
					key={`${state.channel?.id}_m${state.messageId}`}
					channel={state.channel!}
					onSelect={state.onSelect!}
					selectedId={state.selectedId}
					isMobile={true}
					data-flx="personas.persona-picker-mobile-sheet.persona-picker-popout"
				/>
			)}
		</BottomSheet>
	);
});
