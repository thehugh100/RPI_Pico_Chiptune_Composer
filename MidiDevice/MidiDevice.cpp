#define NOMINMAX
#include <SDKDDKVer.h>
#include <Windows.h>

#include <stdio.h>
#include <conio.h>

#include <mmsystem.h>
#include <iostream>

#include <uuids.h>

#pragma comment(lib, "winmm.lib")

#include "RtAudio.h"

#include <fstream>
#include <string>
#include <MidiFile.h>

#include <fixed_point/fixed_point.hpp>

HMIDIIN hMidiDevice = NULL;

std::ofstream file;
bool recording = 0;

void PrintMidiDevices()
{
	UINT nMidiDeviceNum;
	MIDIINCAPS caps;

	nMidiDeviceNum = midiInGetNumDevs();
	if (nMidiDeviceNum == 0) {
		fprintf(stderr, "midiInGetNumDevs() return 0...");
		exit(-1);
	}

	printf("== PrintMidiDevices() == \n");
	for (unsigned int i = 0; i < nMidiDeviceNum; ++i) {
		midiInGetDevCaps(i, &caps, sizeof(MIDIINCAPS));
		//printf("\t%d : name = %s\n", i, caps.szPname);
		std::wcout << i << " : " << caps.szPname << std::endl;
	}
	printf("=====\n");
}


const int sampleRate = 48000;

fp32_t<24> globalMod = fp32_t <24>(0.f);

float noteToFreq(float note) {
	float a = 440.f;
	return (a / 32.f) * pow(2.f, ((note - 9.f) / 12.f));
}

inline float fmodFixed(float a, float b)
{
	return fmod(fmod(a, b) + b, b);
}

#define WAVE_SAW 0
#define WAVE_PLS 1
#define WAVE_SIN 2
#define WAVE_TRI 3
#define WAVE_SAMPLE 4
#define WAVE_SLICER 5

using namespace has_castle;
using namespace has_castle::fixed_point;
using namespace has_castle::fixed_point::detail;

struct Oscilator
{
	Oscilator()
	{
		phase = 0;
		frequency = fp32_t<16>(0);
		volume = fp32_t<24>(0);
		dspVolume = fp32_t <24>(.5f);
		dspFrequency = fp32_t<16>(.5);
		porta = fp32_t<16>(1.f);
		channelPorta = fp32_t <16>(1.f);
		slew = fp32_t<16>(0.01f);
		bend = fp32_t<24>(0);

		duty = fp32_t<24>(.5);

		masterVolume = fp32_t<24>(1);

		originalNote = fp32_t<24>(0);
		note = fp32_t<24>(0);
		noteOn = 0;

		sampleData = nullptr;
		sampleLength = 0;
		sampleCursor = fp32_t<8>{ 0 };
		sampleSize = 0;

		type = WAVE_SAW;
	}

	void loadSample(std::string file)
	{
		std::ifstream fileD(file, std::ios::in | std::ios::binary | std::ios::ate);

		if (!fileD)
		{
			std::cout << "Failed to load: " << file << std::endl;
			return;
		}

		sampleSize = fileD.tellg();
		sampleLength = sampleSize / sizeof(float);

		std::cout << "Loaded Sample: " << file << " (" << sampleLength << " bytes)" << std::endl;

		sampleData = new float[sampleSize];
		fileD.seekg(fileD.beg);
		fileD.read((char*)sampleData, sampleSize);
		fileD.close();
		type = WAVE_SAMPLE;
	}

	fp32_t<16> tick()
	{
		if (type == WAVE_SAMPLE || type == WAVE_SLICER)
		{
			if (sampleLength == 0)
				return fp32_t<16>(0);
		}

		dspFrequency += (fp32_t<4>(frequency) - fp32_t<4>(dspFrequency)) * fp32_t<12>(porta);

		dspVolume += (fp32_t<16>(volume) - fp32_t<16>(dspVolume)) * fp32_t<8>(slew);

		phase += (((uint32_t)dspFrequency) << 16) / sampleRate;

		sampleCursor += fp32_t<16>(frequency) / fp32_t <8>(261.63f);

		duty = fp32_t<24>(0.5) + (fp32_t<12>(globalMod) * fp32_t <12>(.333f));

		fp32_t<16> fVol = fp32_t<8>(volume) * fp32_t<8>(masterVolume);

		switch (type)
		{
		case WAVE_SAW:
			//auto t = (fp32_t<24>(phase) / fp32_t<0>(UINT16_MAX / 2)) - fp32_t<24>(1.f);
			//return t * fVol;
			break;
		case WAVE_PLS:
			return phase > (duty.raw_value() >> 8) ? fp32_t<8>(volume * fp32_t<8>(.5f)) * fp32_t<8>(masterVolume) : fp32_t<8>(-volume * fp32_t<8>(.5f)) * fp32_t<8>(masterVolume);
			break;
		/*case WAVE_TRI:
			return fp32_t<16>(0);
			if ((float)phase < .5f)
				return (-.5f + (float)phase * 2.f) * (float)volume * 2 * (float)masterVolume;
			else
				return (1.5f - (float)phase * 2.f) * (float)volume * 2 * (float)masterVolume;
			break;*/
		//case WAVE_SIN:
		//	return 0;
		//	//return sin(2.f * 3.14159f * (float)phase) * (float)volume * (float)masterVolume;
		//	break;
		/*case WAVE_SAMPLE:
			return sampleData[(uint32_t)sampleCursor % sampleLength] * (float)volume * 3.f * (float)masterVolume;
			break;*/
		/*case WAVE_SLICER:
			return sampleData[(uint32_t)sampleCursor % sampleLength] * (float)volume * 3.f * (float)masterVolume;
			break;*/
		}

		return fp32_t<16>(0);
	}

	uint8_t noteOn;

	fp32_t<24> masterVolume;

	fp32_t<24> originalNote;
	fp32_t<24> note;
	fp32_t<24> bend;

	uint16_t phase;

	fp32_t<16> frequency;
	fp32_t<24> volume;
	fp32_t<24> dspVolume;

	fp32_t<16> dspFrequency;

	fp32_t<16> porta;
	fp32_t<16> channelPorta;
	fp32_t<16> slew;

	fp32_t<24> duty;
	uint8_t type;
	fp32_t<8> sampleCursor;
	//float sampleCursor;

	float* sampleData;
	uint32_t sampleSize;
	uint32_t sampleLength;
};

const int oscilatorsCount = 8;
Oscilator oscilators[oscilatorsCount];

float processOscillators()
{
	float mix = 0.f;
	for (int i = 0; i < oscilatorsCount; ++i)
	{
		mix += (float)oscilators[i].tick() * .3;
	}

	return mix;
}

void handleMidi(uint32_t midi)
{
	uint8_t cmd;
	uint8_t channel;
	uint8_t note;
	uint8_t velocity;

	cmd = (midi & 0x000000FF);
	note = ((midi & 0x0000FF00) >> 8);
	velocity = ((midi & 0x00FF0000) >> 16);

	channel = cmd & 0x0F;
	cmd = cmd & 0xF0;

	if (cmd == 0x90) // note on
	{

		oscilators[channel].note = fp32_t<24>(note);
		oscilators[channel].volume = fp32_t<24>(velocity / 127.f);
		oscilators[channel].frequency = fp32_t<16>(noteToFreq(note + (float)oscilators[channel].bend * 12.f));
		if (oscilators[channel].noteOn == 1) //legato, note was already on
		{
			oscilators[channel].porta = fp32_t<16>(oscilators[channel].channelPorta);
		}
		else
		{
			oscilators[channel].originalNote = fp32_t<24>(note);
		}


		oscilators[channel].noteOn = 1;

		oscilators[channel].sampleCursor = fp32_t<8>(0);

		if (oscilators[channel].type == WAVE_SLICER)
		{
			oscilators[channel].frequency = fp32_t<16>(noteToFreq(60 + (float)oscilators[channel].bend * 12.f));
			uint8_t slice = (note - 60);
			oscilators[channel].sampleCursor = fp32_t<8>{ slice * (oscilators[channel].sampleLength / 16) };
		}
		//std::cout << "Note On: " << std::dec << (int)note << " channel: " << (int)channel << " velocity: " << (int)velocity << std::endl;
	}
	else if (cmd == 0x80) // note off
	{
		if (oscilators[channel].originalNote == fp32_t<24>(note))
		{
			oscilators[channel].volume = fp32_t <24>(0);
			oscilators[channel].noteOn = 0;
			oscilators[channel].porta = fp32_t <16>(1.f);
		}
		else
		{
			
			oscilators[channel].note = oscilators[channel].originalNote;

			if (oscilators[channel].type == WAVE_SLICER)
			{
				oscilators[channel].note = fp32_t <24>(60);
				oscilators[channel].frequency = fp32_t<16>(noteToFreq(60 + (float)oscilators[channel].bend * 12.f));
			}
			else
			{
				oscilators[channel].frequency = fp32_t<16>(noteToFreq((float)oscilators[channel].note + (float)oscilators[channel].bend * 12.f));
			}
		}
		//std::cout << "Note Off: " << std::dec << (int)note << " channel: " << (int)channel << " velocity: " << (int)velocity << std::endl;
	}
	else if (cmd == 0xE0) //pitch bend
	{
		uint16_t bend = (velocity << 7) | (note & 0x7f);
		float bendf = ((float)bend / 8191.5f) - 1.0f;

		oscilators[channel].bend = fp32_t<24>(bendf);
		oscilators[channel].frequency = fp32_t<16>(noteToFreq((float)oscilators[channel].note + bendf * 12.f));

		//std::cout << "pitch bend on channel: " << bendf << std::endl;
	}
	else if (cmd == 0xB0) // MIDI CC
	{
		switch (note) //cc num
		{
		case 7: //channel volume
			oscilators[channel].masterVolume = fp32_t<24>(velocity / 101.6);
			break;
		case 5: //porta
			oscilators[channel].channelPorta = fp32_t <16>(1.0f / (1.f + velocity * 96.f));
			break;
		case 16: //type
			oscilators[channel].type = velocity / 22;
			break;
		}
		//std::cout << "MIDI CC: " << std::dec << (int)note << " channel: " << (int)channel << " value: " << (int)velocity << std::endl;
	}
	else
	{
		std::cout << std::hex << (int)cmd << std::endl;
	}
}

void CALLBACK MidiInProc(HMIDIIN hMidiIn, UINT wMsg, DWORD dwInstance, DWORD dwParam1, DWORD dwParam2)
{
	uint8_t cmd;
	uint8_t note;
	uint8_t channel;
	uint8_t vel;

	switch (wMsg) {
	case MIM_OPEN:
		printf("wMsg=MIM_OPEN\n");
		break;
	case MIM_CLOSE:
		printf("wMsg=MIM_CLOSE\n");
		break;
	case MIM_DATA:
		//printf("wMsg=MIM_DATA, dwInstance=%08x, dwParam1=%08x, dwParam2=%08x\n", dwInstance, dwParam1, dwParam2);

		//std::cout << "MIDI Data: " << std::hex << "0x" << dwParam1 << " : " << dwParam2 << std::dec << std::endl;


		handleMidi(dwParam1);

		break;
	case MIM_LONGDATA:
		printf("wMsg=MIM_LONGDATA\n");
		break;
	case MIM_ERROR:
		printf("wMsg=MIM_ERROR\n");
		break;
	case MIM_LONGERROR:
		printf("wMsg=MIM_LONGERROR\n");
		break;
	case MIM_MOREDATA:
		printf("wMsg=MIM_MOREDATA\n");
		break;
	default:
		printf("wMsg = unknown\n");
		break;
	}
	return;
}

RtAudio::StreamOptions options;
RtAudio dac;
uint32_t frameCount = 0;

void errorCallback(RtAudioError::Type type, const std::string& errorText)
{
	// This example error handling function does exactly the same thing
	// as the embedded RtAudio::error() function.
	std::cout << "in errorCallback" << std::endl;
	if (type == RtAudioError::WARNING)
		std::cerr << '\n' << errorText << "\n\n";
	else if (type != RtAudioError::WARNING)
		throw(RtAudioError(errorText, type));
}


int saw(void* outputBuffer, void* /*inputBuffer*/, unsigned int nBufferFrames,
	double /*streamTime*/, RtAudioStreamStatus status, void* data)
{
	unsigned int i, j;
	unsigned int channels = 2;
	float* buffer = (float*)outputBuffer;

	float* origBuf = (float*)outputBuffer;

	if (status)
		std::cout << "Stream underflow detected!" << std::endl;

	for (i = 0; i < nBufferFrames; ++i)
	{
		float value = processOscillators();

		for (int j = 0; j < channels; ++j)
		{
			*buffer++ = value;
		}
		globalMod = fp32_t<24>(sin(frameCount * 0.00001f));
		frameCount++;
	}
	if (recording)
	{
		file.write((const char*)origBuf, nBufferFrames * 2 * sizeof(float));
		file.flush();
	}
	return 0;
}

void startAudio()
{

	uint32_t deviceCount = 0;
	if ((deviceCount = dac.getDeviceCount()) < 1) {
		std::cout << "\nNo audio devices found!\n";
		exit(1);
	}

	for (int i = 0; i < deviceCount; ++i)
	{
		RtAudio::DeviceInfo d = dac.getDeviceInfo(i);
		std::cout << d.name << ": " << " Sample Rate: " << d.preferredSampleRate << std::endl;
	}

	uint32_t channels = 2;
	uint32_t fs = 48000;
	float* data = (float*)calloc(channels, sizeof(float));

	// Let RtAudio print messages to stderr.
	dac.showWarnings(true);

	// Set our stream parameters for output only.
	uint32_t bufferFrames = 512;

	RtAudio::StreamParameters oParams;
	oParams.deviceId = /*dac.getDefaultOutputDevice()*/ 1;
	oParams.nChannels = channels;
	oParams.firstChannel = 0;

	options.flags = RTAUDIO_SCHEDULE_REALTIME | RTAUDIO_MINIMIZE_LATENCY;
	try {
		dac.openStream(&oParams, NULL, RTAUDIO_FLOAT32, fs, &bufferFrames, &saw, (void*)data, &options, &errorCallback);

		file = std::ofstream("out.bin", std::ios::out | std::ios::binary);

		dac.startStream();
	}
	catch (RtAudioError& e) {
		e.printMessage();
	}

}

void startMidi()
{
	DWORD nMidiPort = 1;
	UINT nMidiDeviceNum;
	MMRESULT rv;

	PrintMidiDevices();

	rv = midiInOpen(&hMidiDevice, nMidiPort, (DWORD_PTR)(void*)MidiInProc, 0, CALLBACK_FUNCTION);
	if (rv != MMSYSERR_NOERROR) {
		fprintf(stderr, "midiInOpen() failed...rv=%d", rv);
		exit(-1);
	}

	midiInStart(hMidiDevice);
}

struct Event
{
	Event(uint32_t midiData, uint16_t delta)
		: midiData(midiData), delta(delta)
	{

	}
	uint32_t midiData;
	uint16_t delta;
};

void playRoutine()
{
	std::vector<Event> events;

	smf::MidiFile midifile;
	midifile.read("chip.mid");
	midifile.doTimeAnalysis();
	midifile.linkNotePairs();

	midifile.joinTracks();
	smf::MidiEvent* mev;
	int deltatick;

	uint32_t routineSize = 0;

	for (int event = 0; event < midifile[0].size(); event++) {
		mev = &midifile[0][event];
		if (event == 0) deltatick = mev->tick;
		else deltatick = mev->tick - midifile[0][event - 1].tick;

		if (mev->isController() || mev->isNoteOn() || mev->isNoteOff() || mev->isPitchbend())
		{
			uint8_t p0 = mev->getP0();
			uint8_t p1 = mev->getP1();
			uint8_t p2 = mev->getP2();

			uint32_t midiMsg = (p0) | (p1 << 8) | (p2 << 16);

			events.push_back(Event(midiMsg, mev->tick));
			routineSize += 2 + 4;
		}
	}
	std::cout << "Loaded routine: (" << routineSize << " bytes)" << std::endl;;


	//save routine
	std::ofstream routine("routine.bin", std::ios::binary | std::ios::out);
	if (routine)
	{
		for (int i = 0; i < events.size(); ++i)
		{
			routine.write((const char*)&events[i].delta, 2);
			routine.write((const char*)&events[i].midiData, 4);
		}
		std::cout << "Saved: routine.bin (" << routine.tellp() << " bytes)" << std::endl;
	}
	else
	{
		std::cout << "Unable to save routine" << std::endl;
	}
	routine.close();

	std::cout << std::endl;

	bool playing = 1;

	float tempo = 140.f;
	float quarterNoteMS = 60000.f / tempo;
	float ticksPerQuarterNote = midifile.getTicksPerQuarterNote();
	float tickMS = quarterNoteMS / ticksPerQuarterNote;
	float samplesPerTick = tickMS / (1000.f / sampleRate);

	std::cout << "Tempo: " << tempo << " BPM" << std::endl;
	std::cout << "Quarter note ms: " << quarterNoteMS << " ms" << std::endl;
	std::cout << "TPQ: " << ticksPerQuarterNote << " ticks" << std::endl;
	std::cout << "TickMS: " << tickMS << " ms" << std::endl;
	std::cout << "Samples per tick: " << samplesPerTick << " samples" << std::endl;

	int currentTick = 0;
	int eventIDX = 0;
	int sampleCounter = 0;

	std::ofstream file("out2.bin", std::ios::out | std::ios::binary);
	if (!file)
	{
		std::cout << "Could not open out.bin for writing" << std::endl;
		return ;
	}

	while (playing)
	{
		if (sampleCounter % (int)samplesPerTick == 0)
		{
			if (eventIDX >= events.size())
				break;

			for (int i = eventIDX; i < events.size(); ++i)
			{
				if (events[i].delta <= currentTick)
				{
					eventIDX = i + 1;
					handleMidi(events[i].midiData);
				}
			}
			currentTick++;
		}

		float value = processOscillators();
		file.write((const char*)&value, 4);
		//uint8_t b = (value * 128.f) + 127.f;
		//file.write((const char*)&b, 1);

		globalMod = fp32_t<24>(sin(sampleCounter * 0.00001f));

		sampleCounter++;
	}

	file.close();
	std::cout << "Finished Play Routine" << std::endl;
	exit(0);
}

int main(int argc, char* argv[])
{
	oscilators[7].loadSample("think.raw");

	//playRoutine();

	startAudio();

	startMidi();

	while (true) {
		if (!_kbhit()) {
			Sleep(100);
			continue;
		}
		int c = _getch();
		if (c == VK_ESCAPE) break;
		if (c == 'q') break;
		if (c == 'r') recording = 1;
	}

	dac.closeStream();

	file.close();

	midiInStop(hMidiDevice);
	midiInClose(hMidiDevice);
	hMidiDevice = NULL;

	return 0;
}