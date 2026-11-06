import React, { useState } from 'react';

export interface EventAttendee {
  id: string;
  name: string;
  avatarUrl: string;
}

export interface FluxerEvent {
  id: string;
  title: string;
  creatorName: string;
  description: string;
  locationChannelName: string;
  hasChannelAccess: boolean;
  startTime: string; 
  endTime: string;   
  attendees: EventAttendee[];
}

interface EventModalProps {
  event: FluxerEvent;
  currentUserId: string;
  onClose: () => void;
  onToggleRSVP: (eventId: string, isAttending: boolean) => void;
}

export const EventModal: React.FC<EventModalProps> = ({
  event,
  currentUserId,
  onClose,
  onToggleRSVP,
}) => {
  // Eshu - Custom tracking for event attendance toggle
  const isAttending = event.attendees.some((a) => a.id === currentUserId);

  const formatLocalTime = (isoString: string) => {
    return new Date(isoString).toLocaleString([], {
      weekday: 'short',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  const handleRSVPToggle = () => {
    onToggleRSVP(event.id, !isAttending);
  };

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 backdrop-blur-sm p-4">
      <div className="w-full max-w-md rounded-xl bg-gray-900 border border-gray-800 text-white shadow-2xl p-6 relative">
        <button
          onClick={onClose}
          className="absolute top-4 right-4 text-gray-400 hover:text-white text-lg"
        >
          ✕
        </button>

        <h2 className="text-xl font-bold text-indigo-400">{event.title}</h2>
        <p className="text-xs text-gray-400 mt-1">Created by {event.creatorName}</p>

        {!event.hasChannelAccess ? (
          <div className="mt-4 p-3 bg-red-950/50 border border-red-800 rounded-lg text-red-300 text-sm">
            You do not have access to the event channel.
          </div>
        ) : (
          <>
            <div className="mt-4 space-y-2 text-sm text-gray-300">
              <div className="flex items-center gap-2">
                <span className="font-semibold text-gray-400">Time:</span>
                <span>
                  {formatLocalTime(event.startTime)} - {formatLocalTime(event.endTime)}
                </span>
              </div>
              <div className="flex items-center gap-2">
                <span className="font-semibold text-gray-400">Location:</span>
                <span className="text-indigo-300 font-medium">#{event.locationChannelName}</span>
              </div>
            </div>

            <p className="mt-4 text-sm text-gray-400 leading-relaxed bg-gray-950 p-3 rounded-lg">
              {event.description}
            </p>

            <div className="mt-6 flex items-center justify-between">
              <button
                onClick={handleRSVPToggle}
                className={`px-5 py-2.5 rounded-lg text-sm font-semibold transition-all shadow-md ${
                  isAttending
                    ? 'bg-emerald-600 hover:bg-emerald-700 text-white ring-2 ring-emerald-400/50'
                    : 'bg-indigo-600 hover:bg-indigo-700 text-white'
                }`}
              >
                {isAttending ? "✓ I'm Going" : "I'm Going?"}
              </button>

              <div className="flex items-center -space-x-2 overflow-hidden">
                {event.attendees.map((attendee) => (
                  <img
                    key={attendee.id}
                    src={attendee.avatarUrl}
                    alt={attendee.name}
                    title={attendee.name}
                    className="inline-block h-8 w-8 rounded-full ring-2 ring-gray-900 object-cover"
                  />
                ))}
              </div>
            </div>
          </>
        )}
      </div>
    </div>
  );
};
          
