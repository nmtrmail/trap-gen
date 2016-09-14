################################################################################
#
#  _/_/_/_/_/  _/_/_/           _/        _/_/_/
#     _/      _/    _/        _/_/       _/    _/
#    _/      _/    _/       _/  _/      _/    _/
#   _/      _/_/_/        _/_/_/_/     _/_/_/
#  _/      _/    _/     _/      _/    _/
# _/      _/      _/  _/        _/   _/
#
# @file     portsWriter.py
# @brief    This file is part of the TRAP processor generator module.
# @details
# @author   Luca Fossati
# @author   Lillian Tadros (Technische Universitaet Dortmund)
# @date     2008-2013 Luca Fossati
#           2015-2016 Technische Universitaet Dortmund
# @copyright
#
# This file is part of TRAP.
#
# TRAP is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
# or see <http://www.gnu.org/licenses/>.
#
# (c) Luca Fossati, fossati@elet.polimi.it, fossati.l@gmail.com
#
################################################################################

import cxx_writer
from registerWriter import registerContainerType

################################################################################
# PIN Classes
################################################################################
def getCPPPINPorts(self, namespace):
    """Returns the classes implementing pins for communicating with the external
    world. There are both incoming and outgoing external ports. For the
    outgoing, I simply have to declare the port class (like memory ports). For
    the incoming, I also have to specify the operation which has to be performed
    when the port is triggered (they are like interrupt ports)."""

    if len(self.pins) == 0:
        return None

    tlmDmiType = cxx_writer.Type('tlm::tlm_dmi', 'tlm.h')

    SCInPorts = []
    SCOutPorts = []
    TLMInPorts = []
    TLMOutPorts = []
    declaredPorts = []

    for port in self.pins:
        # I add all the inbound ports since there is an action specified for
        # each of them. In order to correctly execute the specified action the
        # port needs references to all the architectural elements.
        if port.inbound:
            if port.systemc:
                SCInPorts.append(port)
            else:
                TLMInPorts.append(port)
        else:
            if not (str(port.portWidth) + '_' + str(port.systemc)) in declaredPorts:
                if port.systemc:
                    SCOutPorts.append(port)
                else:
                    TLMOutPorts.append(port)
                declaredPorts.append(str(port.portWidth) + '_' + str(self.systemc))

    pinClasses = []

    # Outbound TLM Ports
    for port in TLMOutPorts:
        pinPortType = cxx_writer.Type('TLMOutPin_' + str(port.portWidth))
        tlmPayloadType = cxx_writer.Type('tlm::tlm_generic_payload', 'tlm.h')
        tlmInitSocketType = cxx_writer.TemplateType('tlm_utils::multi_passthrough_initiator_socket', [pinPortType, port.portWidth, 'tlm::tlm_base_protocol_types', 1, 'sc_core::SC_ZERO_OR_MORE_BOUND'], 'tlm_utils/multi_passthrough_initiator_socket.h')

        pinPortMembers = []
        pinPortCtorParams = []
        pinPortCtorInit = []

        # Methods: send_pin_req()
        sendPinReqBody = cxx_writer.Code("""tlm::tlm_generic_payload trans;
        sc_time delay;
        trans.set_address(address);
        trans.set_write();
        trans.set_data_ptr((unsigned char*)&datum);
        trans.set_data_length(sizeof(datum));
        trans.set_streaming_width(sizeof(datum));
        trans.set_byte_enable_ptr(0);
        trans.set_dmi_allowed(false);
        trans.set_response_status(tlm::TLM_INCOMPLETE_RESPONSE);
        this->init_socket->b_transport(trans, delay);

        if (trans.is_response_error()) {
            std::string error_str("Error from b_transport, response status = " + trans.get_response_string());
            SC_REPORT_ERROR("TLM-2", error_str.c_str());
        }
        """)
        sendPinReqBody.addInclude('common/report.hpp')
        sendPinReqBody.addInclude('tlm.h')
        from isa import resolveBitType
        PINWidthType = resolveBitType('BIT<' + str(port.portWidth) + '>')
        addressParam = cxx_writer.Parameter('address', PINWidthType.makeRef().makeConst())
        datumParam = cxx_writer.Parameter('datum', PINWidthType)
        sendPinReqMethod = cxx_writer.Method('send_pin_req', sendPinReqBody, cxx_writer.voidType, 'public', [addressParam, datumParam], noException = True)
        pinPortMembers.append(sendPinReqMethod)

        # Attributes and Initialization
        pinPortCtorParams.append(cxx_writer.Parameter('pin_name', cxx_writer.sc_module_nameType))
        pinPortCtorInit.append('sc_module(pin_name)')
        initSocketAttr = cxx_writer.Attribute('init_socket', tlmInitSocketType, 'public')
        pinPortMembers.append(initSocketAttr)
        pinPortCtorInit.append('init_socket(\"init_socket\")')

        # Constructors and Destructors
        Code = 'end_module();'
        pinPortCtor = cxx_writer.Constructor(cxx_writer.Code(Code), 'public', pinPortCtorParams, pinPortCtorInit)

        # Class
        pinPortClass = cxx_writer.ClassDeclaration('TLMOutPin_' + str(port.portWidth), pinPortMembers, [cxx_writer.sc_moduleType], namespaces = [namespace])
        pinPortClass.addDocString(brief = 'Pin Class', detail = 'Defines the pins used by the core for communicating with other modules. Outgoing ports call a given interface of another module, while incoming ports need to define the interface to be used by other modules.')
        pinPortClass.addConstructor(pinPortCtor)
        pinClasses.append(pinPortClass)

    # Outbound SystemC ports.
    for port in SCOutPorts:
        raise Exception('Outbound SystemC ports not yet supported.')

    # Inbound TLM ports.
    for port in TLMInPorts:
        raise Exception('Inbound TLM ports not yet supported.')

    # Inbound SystemC ports.
    for port in SCInPorts:
        raise Exception('Inbound SystemC ports not yet supported.')

    return pinClasses

################################################################################
# Port Classes
################################################################################
def getCPPExternalPorts(self, model, namespace):
    """Returns the code implementing TLM ports for communicating with the
    external world."""

    if len(self.tlmPorts) == 0:
        return None

    archDWordType = self.bitSizes[0]
    archWordType = self.bitSizes[1]
    archHWordType = self.bitSizes[2]
    archByteType = self.bitSizes[3]

    from registerWriter import registerType, aliasType
    memIfType = cxx_writer.Type('MemoryInterface', '#include \"memory.hpp\"')
    tlmMemoryType = cxx_writer.Type('TLMMemory')
    memoryToolsIfType = cxx_writer.TemplateType('MemoryToolsIf', [str(archWordType)], 'common/tools_if.hpp')
    tlmDmiType = cxx_writer.Type('tlm::tlm_dmi', 'tlm.h')
    tlmPayloadType = cxx_writer.Type('tlm::tlm_generic_payload', 'tlm.h')
    tlmPhaseType = cxx_writer.Type('tlm::tlm_phase', 'tlm.h')
    tlmSyncEnumType = cxx_writer.Type('tlm::tlm_sync_enum', 'tlm.h')
    tlmInitSocketType = cxx_writer.TemplateType('tlm_utils::simple_initiator_socket', [tlmMemoryType, self.wordSize*self.byteSize], 'tlm_utils/simple_initiator_socket.h')

    extPortMembers = []
    extPortCtorParams = []
    extPortCtorInit = []
    aliasAttrs = []
    aliasParams = []
    aliasInit = []
    emptyBody = cxx_writer.Code('')

    # Methods: Building Blocks
    checkWatchPointCode = """if (this->debugger != NULL) {
        this->debugger->notify_address(address, sizeof(datum));
    }
    """

    swapEndianessCode = '// Endianess conversion: The processor is always modeled with the host endianess. In case they are different, the endianess is swapped.\n'
    if self.isBigEndian:
        swapEndianessDefine = '#ifdef LITTLE_ENDIAN_BO\n'
    else:
        swapEndianessDefine = '#ifdef BIG_ENDIAN_BO\n'
    swapEndianessCode += swapEndianessDefine + 'this->swap_endianess(datum);\n#endif\n'

    if self.isBigEndian:
        swapDEndianessCode = '#ifdef LITTLE_ENDIAN_BO\n'
    else:
        swapDEndianessCode = '#ifdef BIG_ENDIAN_BO\n'
    swapDEndianessCode += str(archWordType) + ' datum1 = (' + str(archWordType) + ')(datum);\nthis->swap_endianess(datum1);\n'
    swapDEndianessCode += str(archWordType) + ' datum2 = (' + str(archWordType) + ')(datum >> ' + str(self.wordSize*self.byteSize) + ');\nthis->swap_endianess(datum2);\n'
    swapDEndianessCode += 'datum = datum1 | (((' + str(archDWordType) + ')datum2) << ' + str(self.wordSize*self.byteSize) + ');\n#endif\n'

    if model.endswith('AT'):
        # Attributes
        extPortMembers.append(cxx_writer.Attribute('request_in_progress', tlmPayloadType.makePointer(), 'private'))
        extPortMembers.append(cxx_writer.Attribute('end_request_event', cxx_writer.sc_eventType, 'private'))
        extPortMembers.append(cxx_writer.Attribute('end_response_event', cxx_writer.sc_eventType, 'private'))

        # Methods: nb_transport_bw()
        Code = """// TLM-2 backward non-blocking transport method.
            // The timing annotation must be honored.
            m_peq.notify(trans, phase, delay);
            return tlm::TLM_ACCEPTED;
            """
        transParam = cxx_writer.Parameter('trans', tlmPayloadType.makeRef())
        phaseParam = cxx_writer.Parameter('phase', tlmPhaseType.makeRef())
        delayParam = cxx_writer.Parameter('delay', cxx_writer.sc_timeType.makeRef())
        helperMethod = cxx_writer.Method('nb_transport_bw', cxx_writer.Code(Code), tlmSyncEnumType, 'public', [transParam, phaseParam, delayParam], inline = True, noException = True)
        extPortMembers.append(helperMethod)

        # Methods: peq_cb()
        Code = """// Payload event queue callback to handle transactions from target.
            // Transaction could have arrived through return path or backward path.
            if (phase == tlm::END_REQ || (&trans == request_in_progress && phase == tlm::BEGIN_RESP)) {
                // The end of the BEGIN_REQ phase.
                request_in_progress = NULL;
                end_request_event.notify();
            } else if (phase == tlm::BEGIN_REQ || phase == tlm::END_RESP) {
                SC_REPORT_FATAL("TLM-2", "Illegal transaction phase received by initiator");
            }

            if (phase == tlm::BEGIN_RESP) {
                if (trans.is_response_error()) {
                    SC_REPORT_ERROR("TLM-2", ("Transaction returned with error, response status = " + trans.get_response_string()).c_str());
                }

                // Send final phase transition to target.
                tlm::tlm_phase fw_phase = tlm::END_RESP;
                sc_time delay = SC_ZERO_TIME;
                init_socket->nb_transport_fw(trans, fw_phase, delay);
                if (trans.is_response_error()) {
                    SC_REPORT_ERROR("TLM-2", ("Transaction returned with error, response status = " + \
                        trans.get_response_string()).c_str());
                }
                this->end_response_event.notify(delay);
            }
            """
        phaseParam = cxx_writer.Parameter('phase', tlmPhaseType.makeRef().makeConst())
        helperMethod = cxx_writer.Method('peq_cb', cxx_writer.Code(Code), cxx_writer.voidType, 'public', [transParam, phaseParam])
        extPortMembers.append(helperMethod)

    # Methods: read()
    if model.endswith('LT'):
        readCode = """ datum = 0;
            if (this->dmi_ptr_valid) {
                if (address + this->dmi_data.get_start_address() > this->dmi_data.get_end_address()) {
                    SC_REPORT_ERROR("TLM-2", "Error in reading memory data through DMI: address out of bounds");
                }
                memcpy(&datum, this->dmi_data.get_dmi_ptr() - this->dmi_data.get_start_address() + address, sizeof(datum));
            """
        if not model.startswith('acc'):
            readCode += """this->quant_keeper.inc(this->dmi_data.get_read_latency());
            if (this->quant_keeper.need_sync()) {
                this->quant_keeper.sync();
            }
            """
        else:
            readCode += 'wait(this->dmi_data.get_read_latency());'
        readCode += """
            } else {
            """
        if not model.startswith('acc'):
            readCode += 'sc_time delay = this->quant_keeper.get_local_time();'
        else:
            readCode += 'sc_time delay = SC_ZERO_TIME;'
        readCode += """
                tlm::tlm_generic_payload trans;
                trans.set_address(address);
                trans.set_read();
                trans.set_data_ptr(reinterpret_cast<unsigned char*>(&datum));
                trans.set_data_length(sizeof(datum));
                trans.set_streaming_width(sizeof(datum));
                trans.set_byte_enable_ptr(0);
                trans.set_dmi_allowed(false);
                trans.set_response_status(tlm::TLM_INCOMPLETE_RESPONSE);
                this->init_socket->b_transport(trans, delay);

                if (trans.is_response_error()) {
                    std::string error_str("Error from b_transport, response status = " + trans.get_response_string());
                    SC_REPORT_ERROR("TLM-2", error_str.c_str());
                }
                if (trans.is_dmi_allowed()) {
                    this->dmi_data.init();
                    this->dmi_ptr_valid = this->init_socket->get_direct_mem_ptr(trans, this->dmi_data);
                }
                // Keep track of time.
            """
        if not model.startswith('acc'):
            readCode += """this->quant_keeper.set(delay);
                if (this->quant_keeper.need_sync()) {
                    this->quant_keeper.sync();
                }
            }
            """
        else:
            readCode += 'wait(delay);\n}\n'
    else:
        readCode = """ datum = 0;
        tlm::tlm_generic_payload trans;
        trans.set_address(address);
        trans.set_read();
        trans.set_data_ptr(reinterpret_cast<unsigned char*>(&datum));
        trans.set_data_length(sizeof(datum));
        trans.set_streaming_width(sizeof(datum));
        trans.set_byte_enable_ptr(0);
        trans.set_dmi_allowed(false);
        trans.set_response_status(tlm::TLM_INCOMPLETE_RESPONSE);

        if (this->request_in_progress != NULL) {
            wait(this->end_request_event);
        }
        request_in_progress = &trans;

        // Forward non-blocking transport method.
        sc_time delay = SC_ZERO_TIME;
        tlm::tlm_phase phase = tlm::BEGIN_REQ;
        tlm::tlm_sync_enum status;
        status = init_socket->nb_transport_fw(trans, phase, delay);

        if (trans.is_response_error()) {
            std::string error_str("Error from nb_transport_fw, response status = " + trans.get_response_string());
            SC_REPORT_ERROR("TLM-2", error_str.c_str());
        }

        // Check value returned from nb_transport_fw().
        if (status == tlm::TLM_UPDATED) {
            // The timing annotation must be honored.
            m_peq.notify(trans, phase, delay);
            wait(this->end_response_event);
        } else if (status == tlm::TLM_COMPLETED) {
            // The completion of the transaction necessarily ends the BEGIN_REQ phase.
            this->request_in_progress = NULL;
            // The target has terminated the transaction, check the correctness.
            if (trans.is_response_error()) {
                SC_REPORT_ERROR("TLM-2", ("Transaction returned with error, response status = " + trans.get_response_string()).c_str());
            }
        }
        wait(this->end_response_event);
        """

    readMemAliasCode = ''
    for alias in self.memAlias:
        readMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\nreturn this->' + alias.alias + ';\n}\n'
    addressParam = cxx_writer.Parameter('address', archWordType.makeRef().makeConst())
    readBody = cxx_writer.Code(readMemAliasCode + str(archDWordType) + readCode + swapDEndianessCode + '\nreturn datum;')
    readBody.addInclude('common/report.hpp')
    readBody.addInclude('tlm.h')
    readMethod = cxx_writer.Method('read_dword', readBody, archDWordType, 'public', [addressParam], noException = True)
    extPortMembers.append(readMethod)

    readBody = cxx_writer.Code(readMemAliasCode + str(archWordType) + readCode + swapEndianessCode + '\nreturn datum;')
    readMethod = cxx_writer.Method('read_word', readBody, archWordType, 'public', [addressParam], inline = True, noException = True)
    extPortMembers.append(readMethod)

    readMemAliasCode = ''
    for alias in self.memAlias:
        readMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn (' + str(archHWordType) + ')' + alias.alias + '_temp;\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + self.wordSize/2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archHWordType) + ' *)&(' + alias.alias + '_temp)) + 1);\n}\n'
    readBody = cxx_writer.Code(readMemAliasCode + str(archHWordType) + readCode + swapEndianessCode + '\nreturn datum;')
    readMethod = cxx_writer.Method('read_half', readBody, archHWordType, 'public', [addressParam], noException = True)
    extPortMembers.append(readMethod)

    readMemAliasCode = ''
    for alias in self.memAlias:
        readMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn (' + str(archByteType) + ')' + alias.alias + '_temp;\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + 1) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archByteType) + ' *)&(' + alias.alias + '_temp)) + 1);\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + 2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archByteType) + ' *)&(' + alias.alias + '_temp)) + 2);\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + 3) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archByteType) + ' *)&(' + alias.alias + '_temp)) + 3);\n}\n'
    readBody = cxx_writer.Code(readMemAliasCode + str(archByteType) + readCode + '\nreturn datum;')
    readMethod = cxx_writer.Method('read_byte', readBody, archByteType, 'public', [addressParam], noException = True)
    extPortMembers.append(readMethod)

    # Methods: write()
    writeCode = ''
    if model.endswith('LT'):
        writeCode += """if (this->dmi_ptr_valid) {
                if (address + this->dmi_data.get_start_address() > this->dmi_data.get_end_address()) {
                    SC_REPORT_ERROR("TLM-2", "Error in writing memory data through DMI: address out of bounds");
                }
                memcpy(this->dmi_data.get_dmi_ptr() - this->dmi_data.get_start_address() + address, &datum, sizeof(datum));
            """
        if not model.startswith('acc'):
            writeCode += """this->quant_keeper.inc(this->dmi_data.get_write_latency());
            if (this->quant_keeper.need_sync()) {
                this->quant_keeper.sync();
            }"""
        else:
            writeCode += 'wait(this->dmi_data.get_write_latency());'
        writeCode += """
            } else {
            """
        if not model.startswith('acc'):
            writeCode += 'sc_time delay = this->quant_keeper.get_local_time();'
        else:
            writeCode += 'sc_time delay = SC_ZERO_TIME;'
        writeCode += """
                tlm::tlm_generic_payload trans;
                trans.set_address(address);
                trans.set_write();
                trans.set_data_ptr((unsigned char*)&datum);
                trans.set_data_length(sizeof(datum));
                trans.set_streaming_width(sizeof(datum));
                trans.set_byte_enable_ptr(0);
                trans.set_dmi_allowed(false);
                trans.set_response_status(tlm::TLM_INCOMPLETE_RESPONSE);
                this->init_socket->b_transport(trans, delay);

                if (trans.is_response_error()) {
                    std::string error_str("Error from b_transport, response status = " + trans.get_response_string());
                    SC_REPORT_ERROR("TLM-2", error_str.c_str());
                }
                if (trans.is_dmi_allowed()) {
                    this->dmi_data.init();
                    this->dmi_ptr_valid = this->init_socket->get_direct_mem_ptr(trans, this->dmi_data);
                }
                // Keep track of time.
            """
        if not model.startswith('acc'):
            writeCode += """this->quant_keeper.set(delay);
                if (this->quant_keeper.need_sync()) {
                    this->quant_keeper.sync();
                }
            }
            """
        else:
            writeCode += 'wait(delay);\n}\n'
    else:
        writeCode += """tlm::tlm_generic_payload trans;
        trans.set_address(address);
        trans.set_write();
        trans.set_data_ptr((unsigned char*)&datum);
        trans.set_data_length(sizeof(datum));
        trans.set_streaming_width(sizeof(datum));
        trans.set_byte_enable_ptr(0);
        trans.set_dmi_allowed(false);
        trans.set_response_status(tlm::TLM_INCOMPLETE_RESPONSE);

        if (this->request_in_progress != NULL) {
            wait(this->end_request_event);
        }
        request_in_progress = &trans;

        // Forward non-blocking transport method.
        sc_time delay = SC_ZERO_TIME;
        tlm::tlm_phase phase = tlm::BEGIN_REQ;
        tlm::tlm_sync_enum status;
        status = init_socket->nb_transport_fw(trans, phase, delay);

        if (trans.is_response_error()) {
            std::string error_str("Error from nb_transport_fw, response status = " + trans.get_response_string());
            SC_REPORT_ERROR("TLM-2", error_str.c_str());
        }

        // Check value returned from nb_transport_fw().
        if (status == tlm::TLM_UPDATED) {
            // The timing annotation must be honored.
            m_peq.notify(trans, phase, delay);
            wait(this->end_response_event);
        } else if (status == tlm::TLM_COMPLETED) {
            // The completion of the transaction necessarily ends the BEGIN_REQ phase.
            this->request_in_progress = NULL;
            // The target has terminated the transaction, check the correctness.
            if (trans.is_response_error()) {
                SC_REPORT_ERROR("TLM-2", ("Transaction returned with error, response status = " + trans.get_response_string()).c_str());
            }
        }
        wait(this->end_response_event);
        """

    writeMemAliasCode = ''
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n this->' + alias.alias + ' = datum;\nreturn;\n}\n'
    writeBody = cxx_writer.Code(swapDEndianessCode + writeMemAliasCode + checkWatchPointCode + writeCode)
    datumParam = cxx_writer.Parameter('datum', archDWordType)
    writeMethod = cxx_writer.Method('write_dword', writeBody, cxx_writer.voidType, 'public', [addressParam, datumParam], noException = True)
    extPortMembers.append(writeMethod)

    writeBody = cxx_writer.Code(swapEndianessCode + writeMemAliasCode + checkWatchPointCode + writeCode)
    datumParam = cxx_writer.Parameter('datum', archWordType)
    writeMethod = cxx_writer.Method('write_word', writeBody, cxx_writer.voidType, 'public', [addressParam, datumParam], inline = True, noException = True)
    extPortMembers.append(writeMethod)

    writeMemAliasCode = swapEndianessDefine
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + self.wordSize/2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) + 1) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#else\n'
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + self.wordSize/2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) + 1) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#endif\n'
    writeBody = cxx_writer.Code(swapEndianessCode + writeMemAliasCode + checkWatchPointCode + writeCode)
    datumParam = cxx_writer.Parameter('datum', archHWordType)
    writeMethod = cxx_writer.Method('write_half', writeBody, cxx_writer.voidType, 'public', [addressParam, datumParam], noException = True)
    extPortMembers.append(writeMethod)

    writeMemAliasCode = swapEndianessDefine
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 3) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archByteType) + '*)&' + alias.alias + '_temp) = (' + str(archByteType) + ')datum;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 1) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 1) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 2) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 3) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#else\n'
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archByteType) + '*)&' + alias.alias + '_temp) = (' + str(archByteType) + ')datum;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 1) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 1) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 2) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 3) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + '*)&' + alias.alias + '_temp) + 3) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#endif\n'
    writeBody = cxx_writer.Code(writeMemAliasCode + checkWatchPointCode + writeCode)
    datumParam = cxx_writer.Parameter('datum', archByteType)
    writeMethod = cxx_writer.Method('write_byte', writeBody, cxx_writer.voidType, 'public', [addressParam, datumParam], noException = True)
    extPortMembers.append(writeMethod)

    # Methods: read_dbg()
    readCode1 = """tlm::tlm_generic_payload trans;
        trans.set_address(address);
        trans.set_read();
        """
    readCode2 = """trans.set_data_ptr(reinterpret_cast<unsigned char*>(&datum));
        this->init_socket->transport_dbg(trans);
        """
    readMemAliasCode = ''
    for alias in self.memAlias:
        readMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\nreturn this->' + alias.alias + ';\n}\n'
    addressParam = cxx_writer.Parameter('address', archWordType.makeRef().makeConst())
    readBody = cxx_writer.Code(readMemAliasCode + readCode1 + 'trans.set_data_length(' + str(self.wordSize*2) + ');\ntrans.set_streaming_width(' + str(self.wordSize*2) + ');\n' + str(archDWordType) + ' datum = 0;\n' + readCode2 + swapDEndianessCode + 'return datum;')
    readBody.addInclude('common/report.hpp')
    readBody.addInclude('tlm.h')
    readMethod = cxx_writer.Method('read_dword_dbg', readBody, archDWordType, 'public', [addressParam], noException = True)
    extPortMembers.append(readMethod)

    readBody = cxx_writer.Code(readMemAliasCode + readCode1 + 'trans.set_data_length(' + str(self.wordSize) + ');\ntrans.set_streaming_width(' + str(self.wordSize) + ');\n' + str(archWordType) + ' datum = 0;\n' + readCode2 + swapEndianessCode + 'return datum;')
    readMethod = cxx_writer.Method('read_word_dbg', readBody, archWordType, 'public', [addressParam], noException = True)
    extPortMembers.append(readMethod)

    readMemAliasCode = ''
    for alias in self.memAlias:
        readMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn (' + str(archHWordType) + ')' + alias.alias + '_temp;\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + self.wordSize/2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archHWordType) + ' *)&(' + alias.alias + '_temp)) + 1);\n}\n'
    readBody = cxx_writer.Code(readMemAliasCode + readCode1 + 'trans.set_data_length(' + str(self.wordSize/2) + ');\ntrans.set_streaming_width(' + str(self.wordSize/2) + ');\n' + str(archHWordType) + ' datum = 0;\n' + readCode2 + swapEndianessCode + 'return datum;')
    readMethod = cxx_writer.Method('read_half_dbg', readBody, archHWordType, 'public', [addressParam], noException = True)
    extPortMembers.append(readMethod)

    readMemAliasCode = ''
    for alias in self.memAlias:
        readMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn (' + str(archByteType) + ')' + alias.alias + '_temp;\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + 1) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archByteType) + ' *)&(' + alias.alias + '_temp)) + 1);\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + 2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archByteType) + ' *)&(' + alias.alias + '_temp)) + 2);\n}\n'
        readMemAliasCode += 'if (address == ' + hex(long(alias.address) + 3) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n' + swapEndianessDefine + 'this->swap_endianess(' + alias.alias + '_temp);\n#endif\nreturn *(((' + str(archByteType) + ' *)&(' + alias.alias + '_temp)) + 3);\n}\n'
    readBody = cxx_writer.Code(readMemAliasCode + readCode1 + 'trans.set_data_length(1);\ntrans.set_streaming_width(1);\n' + str(archByteType) + ' datum = 0;\n' + readCode2 + 'return datum;')
    readMethod = cxx_writer.Method('read_byte_dbg', readBody, archByteType, 'public', [addressParam], noException = True)
    extPortMembers.append(readMethod)

    # Methods: write_dbg()
    writeCode1 = """tlm::tlm_generic_payload trans;
        trans.set_address(address);
        trans.set_write();
        """
    writeCode2 = """trans.set_data_ptr((unsigned char*)&datum);
        this->init_socket->transport_dbg(trans);
        """
    writeMemAliasCode = ''
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n this->' + alias.alias + ' = datum;\nreturn;\n}\n'
    writeBody = cxx_writer.Code(swapDEndianessCode + writeMemAliasCode + writeCode1 + 'trans.set_data_length(' + str(self.wordSize*2) + ');\ntrans.set_streaming_width(' + str(self.wordSize*2) + ');\n' + writeCode2)
    datumParam = cxx_writer.Parameter('datum', archDWordType)
    writeMethod = cxx_writer.Method('write_dword_dbg', writeBody, cxx_writer.voidType, 'public', [addressParam, datumParam], noException = True)
    extPortMembers.append(writeMethod)

    writeBody = cxx_writer.Code(swapEndianessCode + writeMemAliasCode + writeCode1 + 'trans.set_data_length(' + str(self.wordSize) + ');\ntrans.set_streaming_width(' + str(self.wordSize) + ');\n' + writeCode2)
    datumParam = cxx_writer.Parameter('datum', archWordType)
    writeMethod = cxx_writer.Method('write_word_dbg', writeBody, cxx_writer.voidType, 'public', [addressParam, datumParam], noException = True)
    extPortMembers.append(writeMethod)

    writeMemAliasCode = swapEndianessDefine
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + self.wordSize/2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archHWordType) + ' *)' + alias.alias + '_temp) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) + 1) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#else\n'
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archHWordType) + ' *)' + alias.alias + '_temp) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + self.wordSize/2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archHWordType) + ' *)&' + alias.alias + '_temp) + 1) = (' + str(archHWordType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#endif\n'
    writeBody = cxx_writer.Code(swapEndianessCode + writeMemAliasCode + writeCode1 + 'trans.set_data_length(' + str(self.wordSize/2) + ');\ntrans.set_streaming_width(' + str(self.wordSize/2) + ');\n' + writeCode2)
    datumParam = cxx_writer.Parameter('datum', archHWordType)
    writeMethod = cxx_writer.Method('write_half_dbg', writeBody, cxx_writer.voidType, 'public', [addressParam, datumParam], noException = True)
    extPortMembers.append(writeMethod)

    writeMemAliasCode = swapEndianessDefine
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 3) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archByteType) + ' *)&' + alias.alias + '_temp) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + ' *)&' + alias.alias + '_temp) + 1) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 1) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + ' *)&' + alias.alias + '_temp) + 2) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + ' *)&' + alias.alias + '_temp) + 3) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#else\n'
    for alias in self.memAlias:
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address)) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*((' + str(archByteType) + ' *)&' + alias.alias + '_temp) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 1) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + ' *)&' + alias.alias + '_temp) + 1) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 2) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + ' *)&' + alias.alias + '_temp) + 2) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
        writeMemAliasCode += 'if (address == ' + hex(long(alias.address) + 3) + ') {\n' + str(archWordType) + ' ' + alias.alias + '_temp = this->' + alias.alias + ';\n*(((' + str(archByteType) + ' *)&' + alias.alias + '_temp) + 3) = (' + str(archByteType) + ')datum;\nthis->' + alias.alias + '= ' + alias.alias + '_temp;\nreturn;\n}\n'
    writeMemAliasCode += '#endif\n'
    writeBody = cxx_writer.Code(writeMemAliasCode + writeCode1 + 'trans.set_data_length(1);\ntrans.set_streaming_width(1);\n' + writeCode2)
    datumParam = cxx_writer.Parameter('datum', archByteType)
    writeMethod = cxx_writer.Method('write_byte_dbg', writeBody, cxx_writer.voidType, 'public', [addressParam, datumParam], noException = True)
    extPortMembers.append(writeMethod)

    # Methods: lock(), unlock()
    lockMethod = cxx_writer.Method('lock', emptyBody, cxx_writer.voidType, 'public')
    extPortMembers.append(lockMethod)
    unlockMethod = cxx_writer.Method('unlock', emptyBody, cxx_writer.voidType, 'public')
    extPortMembers.append(unlockMethod)

    # Methods: set_debugger()
    Code = 'this->debugger = debugger;'
    extPortMembers.append(cxx_writer.Method('set_debugger', cxx_writer.Code(Code), cxx_writer.voidType, 'public', [cxx_writer.Parameter('debugger', memoryToolsIfType.makePointer())]))

    # Attributes and Initialization
    if self.memAlias:
        aliasAttrs.append(cxx_writer.Attribute('R', registerContainerType.makeRef(), 'private'))
        aliasParams.append(cxx_writer.Parameter('R', registerContainerType.makeRef()))
        aliasInit.append('R(R)')

    extPortMembers.append(cxx_writer.Attribute('debugger', memoryToolsIfType.makePointer(), 'private'))
    extPortCtorCode = 'this->debugger = NULL;\n'

    initSocketAttr = cxx_writer.Attribute('init_socket', tlmInitSocketType, 'public')
    extPortMembers.append(initSocketAttr)

    extPortCtorParams.append(cxx_writer.Parameter('port_name', cxx_writer.sc_module_nameType))
    extPortCtorInit.append('sc_module(port_name)')

    if model.endswith('LT'):
        if not model.startswith('acc'):
            quantumKeeperType = cxx_writer.Type('tlm_utils::tlm_quantumkeeper', 'tlm_utils/tlm_quantumkeeper.h')
            quantumKeeperAttr = cxx_writer.Attribute('quant_keeper', quantumKeeperType.makeRef(), 'private')
            extPortMembers.append(quantumKeeperAttr)
            extPortCtorParams.append(cxx_writer.Parameter('quant_keeper', quantumKeeperType.makeRef()))
            extPortCtorInit.append('quant_keeper(quant_keeper)')
        dmiPtrValidAttr = cxx_writer.Attribute('dmi_ptr_valid', cxx_writer.boolType, 'private')
        extPortMembers.append(dmiPtrValidAttr)
        dmiDataAttr = cxx_writer.Attribute('dmi_data', tlmDmiType, 'private')
        extPortMembers.append(dmiDataAttr)
        extPortCtorCode += 'this->dmi_ptr_valid = false;\n'
    else:
        peqType = cxx_writer.TemplateType('tlm_utils::peq_with_cb_and_phase', [tlmMemoryType], 'tlm_utils/peq_with_cb_and_phase.h')
        extPortMembers.append(cxx_writer.Attribute('m_peq', peqType, 'private'))
        extPortCtorInit.append('m_peq(this, &TLMMemory::peq_cb)')
        extPortCtorInit.append('request_in_progress(NULL)')
        extPortCtorCode += """// Register callbacks for incoming interface method calls.
            this->init_socket.register_nb_transport_bw(this, &TLMMemory::nb_transport_bw);
            """

    # Constructors and Destructors
    extPortCtorBody = cxx_writer.Code(extPortCtorCode + 'end_module();')
    extPortCtor = cxx_writer.Constructor(extPortCtorBody, 'public', extPortCtorParams + aliasParams, extPortCtorInit + aliasInit)

    # Class
    extPortClass = cxx_writer.ClassDeclaration('TLMMemory', extPortMembers + aliasAttrs, [memIfType, cxx_writer.sc_moduleType], namespaces = [namespace])
    extPortClass.addDocString(brief = 'Port Class', detail = 'Defines the TLM ports used by the core for communicating with other modules.')
    extPortClass.addConstructor(extPortCtor)

    return extPortClass

################################################################################
# Interrupt Port Classes
################################################################################
def getCPPIRQPorts(self, namespace):
    """Returns the classes implementing the interrupt ports. There can be two
    different kind of ports: SystemC-based or TLM-based."""

    tlmPayloadType = cxx_writer.Type('tlm::tlm_generic_payload', 'tlm.h')
    tlmSyncEnumType = cxx_writer.Type('tlm::tlm_sync_enum', 'tlm.h')

    TLMWidth = []
    SyscWidth = []
    for i in self.irqs:
        if i.operation:
            if i.tlm:
                if not i.portWidth in TLMWidth:
                    TLMWidth.append(i.portWidth)
            else:
                if not i.portWidth in SyscWidth:
                    SyscWidth.append(i.portWidth)

    irqPortClasses = []

    # TLM Ports
    # Declared as normal TLM slaves.
    for portWidth in TLMWidth:
        tlmTargetSocketType = cxx_writer.TemplateType('tlm_utils::multi_passthrough_target_socket', ['TLMIntrPort_' + str(portWidth), str(portWidth), 'tlm::tlm_base_protocol_types', 1, 'sc_core::SC_ZERO_OR_MORE_BOUND'], 'tlm_utils/multi_passthrough_target_socket.h')

        irqPortMembers = []
        irqPortCtorParams = []
        irqPortCtorInit = []
        irqPortCtorCode = ''

        # Methods: b_transport()
        bTransportCode = """unsigned char* ptr = trans.get_data_ptr();
            sc_dt::uint64 adr = trans.get_address();
            if (*ptr == 0) {
                // Lower the interrupt.
                this->irq_signal = -1;
            } else {
                // Raise the interrupt.
                this->irq_signal = adr;
            }
            trans.set_response_status(tlm::TLM_OK_RESPONSE);
        """
        bTransportBody = cxx_writer.Code(bTransportCode)
        tagParam = cxx_writer.Parameter('tag', cxx_writer.intType)
        payloadParam = cxx_writer.Parameter('trans', tlmPayloadType.makeRef())
        delayParam = cxx_writer.Parameter('delay', cxx_writer.sc_timeType.makeRef())
        bTransportMethod = cxx_writer.Method('b_transport', bTransportBody, cxx_writer.voidType, 'public', [tagParam, payloadParam, delayParam])
        irqPortMembers.append(bTransportMethod)

        # Methods: transport_dbg()
        debugTransportBody = cxx_writer.Code(bTransportCode + 'return trans.get_data_length();')
        debugTransportMethod = cxx_writer.Method('transport_dbg', debugTransportBody, cxx_writer.uintType, 'public', [tagParam, payloadParam])
        irqPortMembers.append(debugTransportMethod)

        # Methods: nb_transport_fw()
        nbTransportCode = """THROW_EXCEPTION("Method not yet implemented.");
        return tlm::TLM_COMPLETED;
        """
        nbTransportBody = cxx_writer.Code(nbTransportCode)
        nbTransportBody.addInclude('common/report.hpp')
        phaseParam = cxx_writer.Parameter('phase', cxx_writer.Type('tlm::tlm_phase').makeRef())
        nbTransportMethod = cxx_writer.Method('nb_transport_fw', nbTransportBody, tlmSyncEnumType, 'public', [tagParam, payloadParam, phaseParam, delayParam])
        irqPortMembers.append(nbTransportMethod)

        # Attributes and Initialization
        irqPortCtorParams.append(cxx_writer.Parameter('port_name', cxx_writer.sc_module_nameType))
        irqPortCtorInit.append('sc_module(port_name)')

        from isa import resolveBitType
        widthType = resolveBitType('BIT<' + str(portWidth) + '>')
        irqSignalAttr = cxx_writer.Attribute('irq_signal', widthType.makeRef(), 'public')
        irqPortMembers.append(irqSignalAttr)
        irqPortCtorParams.append(cxx_writer.Parameter('irq_signal', widthType.makeRef()))
        irqPortCtorInit.append('irq_signal(irq_signal)')

        socketAttr = cxx_writer.Attribute('target_socket', tlmTargetSocketType, 'public')
        irqPortMembers.append(socketAttr)
        irqPortCtorInit.append('target_socket(port_name)')
        irqPortCtorCode += 'this->target_socket.register_b_transport(this, &TLMIntrPort_' + str(portWidth) + '::b_transport);\n'
        irqPortCtorCode += 'this->target_socket.register_nb_transport_fw(this, &TLMIntrPort_' + str(portWidth) + '::nb_transport_fw);\n'
        irqPortCtorCode += 'this->target_socket.register_transport_dbg(this, &TLMIntrPort_' + str(portWidth) + '::transport_dbg);\n'

        # Constructors and Destructors
        irqPortCtorBody = cxx_writer.Code(irqPortCtorCode + 'end_module();')
        irqPortCtor = cxx_writer.Constructor(irqPortCtorBody, 'public', irqPortCtorParams, irqPortCtorInit)

        # Class
        irqPortClass = cxx_writer.ClassDeclaration('TLMIntrPort_' + str(portWidth), irqPortMembers, [cxx_writer.sc_moduleType], namespaces = [namespace])
        irqPortClass.addConstructor(irqPortCtor)
        irqPortClasses.append(irqPortClass)

    # SystemC Ports
    # Contain a method sensitive to a signal. Note that in order to lower the
    # interrupt the signal has to be 0.
    for portWidth in SyscWidth:
        irqPortMembers = []
        irqPortCtorParams = []
        irqPortCtorInit = []
        irqPortCtorCode = ''

        # Methods: irq_received()
        irqReceivedCode = 'this->irq_signal = this->irq_received_signal.read();'
        irqReceivedMethod = cxx_writer.Method('irq_received', cxx_writer.Code(irqReceivedCode), cxx_writer.voidType, 'public')
        irqPortMembers.append(irqReceivedMethod)

        # Attributes and Initialization
        irqPortCtorParams.append(cxx_writer.Parameter('port_name', cxx_writer.sc_module_nameType))
        irqPortCtorInit.append('sc_module(port_name)')

        from isa import resolveBitType
        widthType = resolveBitType('BIT<' + str(portWidth) + '>')
        widthSignalType = cxx_writer.TemplateType('sc_signal', [widthType], 'systemc.h')
        irqSignalAttr = cxx_writer.Attribute('irq_signal', widthType.makeRef(), 'public')
        irqPortMembers.append(irqSignalAttr)
        irqPortCtorParams.append(cxx_writer.Parameter('irq_signal', widthType.makeRef()))
        irqPortCtorInit.append('irq_signal(irq_signal)')

        irqSignalAttr = cxx_writer.Attribute('irq_received_signal', widthSignalType, 'public')
        irqPortMembers.append(irqSignalAttr)
        irqPortCtorCode += 'SC_METHOD();\nsensitive << this->irq_received_signal;\n'

        # Constructors and Destructors
        irqPortCtorBody = cxx_writer.Code(irqPortCtorCode + 'end_module();')
        irqPortCtor = cxx_writer.Constructor(irqPortCtor, 'public', irqPortCtorParams, irqPortCtorInit)

        irqPortClass = cxx_writer.ClassDeclaration('SCIntrPort_' + str(portWidth), irqPortMembers, [cxx_writer.sc_moduleType], namespaces = [namespace])
        irqPortClass.addDocString(brief = 'Interrupt Port Class', detail = 'Defines both SystemC and TLM ports for convenience.')
        irqPortClass.addConstructor(irqPortCtor)
        irqPortClasses.append(irqPortClass)

    return irqPortClasses

###############################################################################
# Interrupt Port Test Functions
################################################################################
def getCPPIRQTests(self, trace, combinedTrace, namespace):
    """Returns the code for testing the interrupt ports."""

    # Code common to all tests of a single instruction: Variables are declared
    # and the IRQ port instantiated with stub parameters.
    declCode = ''

    # Registers, Aliases and Register Banks
    if self.regs or self.regBanks:
        from registerWriter import registerContainerType
        declCode += registerContainerType.name + ' R('
        # Register const or reset values could be processor variables.
        # Since we do not have the values for those (probably program-dependent),
        # we pass on zeros to the Registers ctor.
        Code = ''
        for reg in self.regs:
            if isinstance(reg.constValue, str):
                Code += '0, '
            if isinstance(reg.defValue, str):
                Code += '0, '

        for regBank in self.regBanks:
            for regConstValue in regBank.constValue.values():
                if isinstance(regConstValue, str):
                    Code += '0, '
            for regDefaultValue in regBank.defValues:
                if isinstance(regDefaultValue, str):
                    Code += '0, '
        if Code:
            declCode += Code[:-2] + ');\n'
        else:
            declCode += ');\n'
        # We also explicitly reset all regs to zero, instead of the reset value.
        # Test writers tend to mask status registers apart from the bits they
        # are interested in, which is perhaps not quite correct but intuitive.
        declCode += 'R.write_force(0);\n'

    #Code = ''
    #for alias in self.memAlias:
        #Code += ', ' + alias.alias

    # Memory
    if (trace or (self.memory and self.memory[2])) and not self.systemc:
        declCode += 'unsigned total_cycles;\n'
    if self.memory:
        Code = ''
        if self.memory[2]:
            Code += ', total_cycles'
        if self.memory[3]:
            Code += ', ' + self.memory[3]
        declCode += namespace + '::LocalMemory ' + self.memory[0] + '(' + str(self.memory[1]) + Code + ');\n'

    # Ports
    # Local memories are declared even for TLM ports. The default memory size is
    # 1MB.
    for tlmPorts in self.tlmPorts.keys():
        declCode += namespace + '::LocalMemory ' + tlmPorts + '(' + str(1024*1024) + ');\n'

    # Pins
    for pinPort in self.pins:
        if not pinPort.inbound:
            if pinPort.systemc:
                pinPortTypeName = 'SC'
            else:
                pinPortTypeName = 'TLM'
            if pinPort.inbound:
                pinPortTypeName += 'InPin_'
            else:
                pinPortTypeName += 'OutPin_'
            pinPortTypeName += str(pinPort.portWidth)
            declCode += namespace + '::' + pinPortTypeName + ' ' + pinPort.name + '_pin(\"' + pinPort.name + '_pin\");\n'
            declCode += 'PINTarget<' + str(pinPort.portWidth) + '> ' + pinPort.name + '_target_pin(\"' + pinPort.name + '_target_pin\");\n'
            declCode += pinPort.name + '_pin.init_socket.bind(' + pinPort.name + '_target_pin.target_socket);\n'

    # IRQ Tests
    from procWriter import testNames
    irqTestFunctions = []
    for irq in self.irqs:
        from isa import resolveBitType
        irqType = resolveBitType('BIT<' + str(irq.portWidth) + '>')
        declCode += '\n// Fake interrupt line ' + str(irqType) + ' ' + irq.name + ';\n'

        # Individual tests of a single IRQ port.
        testNum = 0
        for test in irq.tests:
            # Instantiate architectural elements.
            irqTestCode = declCode

            # Initialize global resources.
            # Note that each test is composed of two parts: The first contains
            # the status of the processor before the interrupt and the second
            # contains the status after.
            for resource, value in test[0].items():
                bracket = resource.find('[')
                memories = self.tlmPorts.keys()
                if self.memory:
                    memories.append(self.memory[0])
                if bracket > 0 and resource[:bracket] in memories:
                    try:
                        irqTestCode += resource[:bracket] + '.write_word_dbg(' + hex(int(resource[bracket + 1:-1])) + ', ' + hex(value) + ');\n'
                    except ValueError:
                        irqTestCode += resource[:bracket] + '.write_word_dbg(' + hex(int(resource[bracket + 1:-1], 16)) + ', ' + hex(value) + ');\n'
                elif resource == irq.name:
                    irqTestCode += resource + ' = ' + hex(value) + ';\n'
                else:
                    irqTestCode += resource + '.write_force(' + hex(value) + ');\n'

            # Set interrupts.
            irqTestCode += 'if ('
            if (irq.condition):
                irqTestCode += '('
            irqTestCode += irq.name + ' != -1'
            if (irq.condition):
                irqTestCode += ') && (' + irq.condition + ')'
            irqTestCode += ') {\n'

            # Instantiate IRQ instruction under test.
            irqTestCode += + irq.name + 'IntrInstruction test_instruction(' + instrCtorValues + ', ' + irq.name + ');\n'

            # Run IRQ instruction behavior.
            irqTestCode += """try {
                test_instruction.behavior();
            }
            catch(annul_exception& etc) {
            }"""
            irqTestCode += '\n}\n'

            # Test the output values.
            for resource, value in test[1].items():
                irqTestCode += 'BOOST_CHECK_EQUAL('
                bracket = resource.find('[')
                memories = self.tlmPorts.keys()
                if self.memory:
                    memories.append(self.memory[0])
                if bracket > 0 and resource[:bracket] in memories:
                    try:
                        irqTestCode += resource[:bracket] + '.read_word_dbg(' + hex(int(resource[bracket + 1:-1])) + ')'
                    except ValueError:
                        irqTestCode += resource[:bracket] + '.read_word_dbg(' + hex(int(resource[bracket + 1:-1], 16)) + ')'
                elif bracket > 0 and resource[:bracket] in outPinPorts:
                    try:
                        irqTestCode += resource[:bracket] + '_target.read_pin(' + hex(int(resource[bracket + 1:-1])) + ')'
                    except ValueError:
                        irqTestCode += resource[:bracket] + '_target.read_pin(' + hex(int(resource[bracket + 1:-1], 16)) + ')'
                else:
                    irqTestCode += resource + '.read_force()'
                irqTestCode += ', (' + str(self.bitSizes[1]) + ')' + hex(value) + ');\n\n'

            irqTestBody = cxx_writer.Code(irqTestCode)
            irqTestBody.addInclude('#include \"instructions.hpp\"')
            disableWarningsCode = '#ifdef _WIN32\n#pragma warning(disable : 4101)\n#endif\n'
            includeUnprotectedCode = '#define private public\n#define protected public\n#include \"registers.hpp\"\n#include \"memory.hpp\"\n#undef private\n#undef protected\n'
            irqTestBody.addInclude(['boost/test/test_tools.hpp', 'common/report.hpp', disableWarningsCode, includeUnprotectedCode])

            testName = 'irq_test_' + irq.name + '_' + str(testNum)
            irqTestFunction = cxx_writer.Function(testName, irqTestBody, cxx_writer.voidType)
            irqTestFunction.addDocString(brief = 'IRQ Test Function', detail = 'Called by test/main.cpp::main() via the boost::test framework. Instantiates the required modules and tests correct IRQ handling.')
            irqTestFunctions.append(irqTestFunction)
            testNames.append(testName)
            testNum += 1

    return irqTestFunctions

################################################################################
