export interface NativeEngineAudioTrackPumpStats {
	active: boolean;
	captureId: string | null;
	startedAt: number | null;
	lastFrameAt: number | null;
	framesForwarded: number;
	lastFramePeak: number | null;
	lastFrameRms: number | null;
	maxFramePeak: number;
	maxFrameRms: number;
	nonSilentFrameCount: number;
	endReason: string | null;
	endDetail: string | null;
	endedAt: number | null;
}

const initialPumpStats: NativeEngineAudioTrackPumpStats = {
	active: false,
	captureId: null,
	startedAt: null,
	lastFrameAt: null,
	framesForwarded: 0,
	lastFramePeak: null,
	lastFrameRms: null,
	maxFramePeak: 0,
	maxFrameRms: 0,
	nonSilentFrameCount: 0,
	endReason: null,
	endDetail: null,
	endedAt: null,
};

const pumpStats: NativeEngineAudioTrackPumpStats = {...initialPumpStats};

export function getNativeEngineAudioTrackPumpStats(): NativeEngineAudioTrackPumpStats {
	return {...pumpStats};
}
