"""
    This test_suit.py file runs the test suite that comes with the METRo program
"""


import argparse
import json
import os
import subprocess
import xml.etree.ElementTree as ET


num_of_success = 0
num_of_failure = 0
list_of_failure_cases = []
dict_error = {}
list_of_errors = []
error_difference = 0
max_tag = ''
max_value_one = 0
max_value_two = 0


# -------------------------------------Class definition: XmlTree--------------------------------------------------------
class XmlTree:
    sum_of_error_outside_tolerance = 0
    sum_of_error_within_tolerance = 0
    list_of_diff_case = []
    display_info = True

    def __init__(self, error_value=0.01):
        self._error = error_value

    # getter method
    def get_error(self):
        return self._error

    # setter method
    def set_error(self, error_value):
        self._error = error_value

    @staticmethod
    def convert_string_to_tree(xml_file):
        """
            Convert an XML file into a tree
            :param xml_file: the name of the file that is going to be converted
            :return: the tree which is converted from the 'xml_file'
        """
        return (ET.parse(xml_file)).getroot()

    def xml_compare(self, xml_file1, xml_file2, excludes=None, display_info=True):
        """
            Compare two xml trees
            :param display_info: boolean variable to indicate if the program display all the different value of tags
            :param xml_file1: the first tree from xml_file1
            :param xml_file2: the second tree from xml_file2
            :param excludes: list of attributes to exclude from comparison
            :return: True if both files match with each other
        """

        global dict_error
        global list_of_errors
        global error_difference
        global max_tag
        global max_value_one
        global max_value_two

        if excludes is None:
            excludes = []

        if xml_file1.tag != xml_file2.tag:
            if display_info:
                print('Tags do not match: {} and {}.'.format(xml_file1.tag, xml_file2.tag))
            return False

        for name, value in xml_file1.attrib.items():
            if name not in excludes:
                if xml_file2.attrib.get(name) != value:
                    if display_info:
                        print('Attributes do not match: {}={} and {}={}.'.format(name, value, name,
                                                                                 xml_file2.attrib.get(name)))
                    return False

        for name in xml_file2.attrib.keys():
            if name not in excludes:
                if name not in xml_file1.attrib:
                    if display_info:
                        print('xml_file2 has an attribute xml_file1 is missing: {}.'.format(name))
                    return False

        if not self.text_compare(xml_file1.text, xml_file2.text):
            if abs(float(xml_file1.text) - float(xml_file2.text)) - self.get_error() <= 0.00000001:
                XmlTree.sum_of_error_within_tolerance += 1

            elif abs(float(xml_file1.text) - float(xml_file2.text)) - self.get_error() > 0.00000001:
                XmlTree.sum_of_error_outside_tolerance += 1
                if display_info:
                    tag_reference = '<' + xml_file1.tag + '>' + xml_file1.text + '<' + xml_file1.tag + '>'
                    tag_test_suite_run = '<' + xml_file2.tag + '>' + xml_file2.text + '<' + xml_file2.tag + '>'
                    print("{}{}{}".format(tag_reference.ljust(24), '!='.ljust(10), tag_test_suite_run))

                    if xml_file1.tag in dict_error.keys():
                        abs_error_difference = abs(float(xml_file1.text) - float(xml_file2.text))
                        if abs_error_difference - abs(dict_error[xml_file1.tag][0] - dict_error[xml_file1.tag][1]) > 0.00000001:
                            list_of_errors = [float(xml_file1.text), float(xml_file2.text)]
                            dict_error[xml_file1.tag] = list_of_errors
                            if abs_error_difference - error_difference > 0.00000001:
                                error_difference = abs_error_difference
                                max_tag = xml_file1.tag
                                max_value_one = float(xml_file1.text)
                                max_value_two = float(xml_file2.text)

                    elif xml_file1.tag not in dict_error.keys():
                        list_of_errors = [float(xml_file1.text), float(xml_file2.text)]
                        dict_error[xml_file1.tag] = list_of_errors
                        if abs(float(xml_file1.text) - float(xml_file2.text)) - error_difference > 0.00000001:
                            error_difference = abs(float(xml_file1.text) - float(xml_file2.text))
                            max_tag = xml_file1.tag
                            max_value_one = float(xml_file1.text)
                            max_value_two = float(xml_file2.text)

            return False

        if not self.text_compare(xml_file1.tail, xml_file2.tail):
            if display_info:
                print('tail: {} != {}.'.format(xml_file1.tail, xml_file2.tail))
            return False

        child1 = xml_file1.getchildren()
        child2 = xml_file2.getchildren()
        if len(child1) != len(child2):
            if display_info:
                print('children length differs, {} != {}.'.format(len(child1), len(child2)))
            return False

        for c1, c2 in zip(child1, child2):
            if c1.tag not in excludes:
                if not self.xml_compare(c1, c2, excludes, display_info):
                    pass
        return True

    def text_compare(self, text1, text2):
        """
            Compare two text strings
            :param text1: text one
            :param text2: text two
            :return: True if these two text strings are a match
        """
        if not text1 and not text2:
            return True
        if text1 == '*' or text2 == '*':
            return True
        return (text1 or '').strip() == (text2 or '').strip()


# -----------------------------------------Method definition------------------------------------------------------------
def process_case_name(case_string, case_list=None):
    """
        Process the case name(s) that is/are passed by the command line.
        :param case_string: case number entered by the user through command line
        :param case_list: a case list including all case number entered by the user
        :return: a validated case list for further process
    """
    if case_list is None:
        case_list = []
    case_string = ''.join(case_string)
    case_string = list(case_string.split(','))
    for case in case_string:
        if case.startswith('case'):
            case = case[4:]
        case = case.zfill(3)
        case = 'case' + case
        if case not in case_list:
            case_list.append(case)
    return case_list


def process_test_result(case_folder, test_code, expected_value_json):
    """
        Processes the running result(s) of the test suite
        :param case_folder: case name which is being tested
        :param test_code: code returned by the program after done running the case
        :param expected_value_json: predefined value inside 'config.jason' file
        :return: case running result
    """
    global num_of_success
    global num_of_failure
    global list_of_failure_cases

    if (test_code == 0 and expected_value_json == 'SUCCESS' and XmlTree.sum_of_error_outside_tolerance == 0) \
            or (test_code != 0 and expected_value_json == 'FAILURE'):
        num_of_success += 1
        print(case_folder, ' SUCCESS!')
    elif (test_code != 0 and expected_value_json == 'SUCCESS') or (test_code == 0 and expected_value_json == 'FAILURE') \
            or (test_code == 0 and expected_value_json == 'SUCCESS' and XmlTree.sum_of_error_outside_tolerance > 0):
        num_of_failure += 1
        list_of_failure_cases.append(case_folder)
        print(case_folder, ' FAILURE! ***')
    else:
        print('Something went wrong with this test run, please try to restart it again!')


def process_xml_file(current_case_path, case_folder, error_value, verbosity=False):
    """
        Compares the output XML file with the given benchmark XML file
        :param current_case_path: directory of the current case that is going to be compared
        :param case_folder: case number that is being compared
        :param error_value: defined error tolerance for the running case
        :param verbosity: boolean variable to indicate the willingness of display the comparison result in detial
        :return: comparison result
    """

    global dict_error
    global list_of_errors
    global error_difference
    global max_tag
    global max_value_one
    global max_value_two

    os.chdir(current_case_path)
    XmlTree.sum_of_error_within_tolerance = 0
    XmlTree.sum_of_error_outside_tolerance = 0
    comparator = XmlTree(error_value)
    if verbosity:
        XmlTree.display_info = True
    elif not verbosity:
        XmlTree.display_info = False

    try:
        tree1 = XmlTree.convert_string_to_tree('roadcast_reference.xml')
        tree2 = XmlTree.convert_string_to_tree('roadcast_test_suite_run.xml')

        dict_error = {}
        list_of_errors = []
        error_difference = 0.0
        max_tag = ''
        max_value_one = 0.0
        max_value_two = 0.0

        if comparator.xml_compare(tree1, tree2, ['production-date'],
                                  XmlTree.display_info) and XmlTree.sum_of_error_outside_tolerance == 0:
            if verbosity:
                print('\nXML FILES COMPARISON RESULT: \t', end='')
                print('  {} differences within the predefined error tolerance.\n'.
                      format(XmlTree.sum_of_error_within_tolerance))
        elif XmlTree.sum_of_error_outside_tolerance != 0:
            XmlTree.list_of_diff_case.append(case_folder)
            if verbosity:
                print('\nXML FILES COMPARISON RESULT: ', end='')
                print('\t  {} differences within the predefined error tolerance.'
                      .format(str(XmlTree.sum_of_error_within_tolerance).rjust(4)))
                print('                             \t  {} differences outside the predefined error tolerance.\n'
                      .format(str(XmlTree.sum_of_error_outside_tolerance).rjust(4)))

                print('The largest error difference exists in tag <{}> and the error difference is {}.\n'.format(max_tag, round(error_difference, 2)))
                # print('\n                                          roadcast_reference.xml'
                #       '              roadcast_test_suite.xml')
                # print('                                          ----------------------'
                #       '              -----------------------')
                # print('The largest error difference exists in:      <{0}>{1}<{0}>                       <{0}>{2}<{0}>\n'
                #       .format(max_tag, max_value_one, max_value_two))

    except FileNotFoundError:
        pass


# ---------------------------------------Method: main()-----------------------------------------------------------------
def main():
    # ----------------------------------------Process user's input from command line------------------------------------
    case_name = ''
    run_all_cases = True
    verbosity = False
    default_error_tolerance = 0.01
    parser = argparse.ArgumentParser(description='run the test suite')
    parser.add_argument('-c', '--case', nargs='+', default=[], metavar='', help='add case number to a case list')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('-q', '--quiet', action='store_true', help='display the process in a simple shortened way')
    group.add_argument('-v', '--verbose', action='store_true', help='display the process in a complete detailed way')
    parser.add_argument('-e', '--error', type=float, help='specified value of error tolerance')
    parser.add_argument('--clean', action='store_true', help="clean up output XML file 'roadcast_test_suite_run.xml'")
    args = parser.parse_args()

    if args.case:
        run_all_cases = False
        if args.verbose:
            verbosity = True
        elif args.quiet:
            verbosity = False
        else:
            verbosity = False
    elif not args.case:
        case_name = 'case'
        run_all_cases = True
        if args.verbose:
            verbosity = True
        elif args.quiet:
            verbosity = False
        else:
            verbosity = False
    if args.error:
        error_value = args.error
    else:
        error_value = default_error_tolerance

    # --------------------------------------------List of validated test cases------------------------------------------
    if verbosity:
        print('\n\n================================================================================')
        print('\n', '                         ', 'VALIDATED LIST OF TEST CASES: ')
        print('\n================================================================================')
    num_of_folders = 0
    list_of_folders = []
    list_of_wrong_folders = []
    test_suite_path = os.getcwd()
    case_list = process_case_name(args.case)
    if run_all_cases:
        for folder in sorted(os.listdir(test_suite_path)):
            try:
                if folder.startswith(case_name):
                    list_of_folders.append(folder)
                    num_of_folders += 1
                    if verbosity:
                        print(folder)
            except TypeError:
                print('There is no folder by the name that you entered.')
                break
    elif not run_all_cases:
        for case in case_list:
            for folder in os.listdir(test_suite_path):
                if case == folder:
                    list_of_folders.append(case)
                    num_of_folders += 1
                    if verbosity:
                        print(case)

    if (sorted(list_of_folders) != sorted(case_list)) and not run_all_cases:
        for case in case_list:
            for folder in list_of_folders:
                if case not in list_of_folders and case not in list_of_wrong_folders:
                    list_of_wrong_folders.append(case)
        print('\nWarning: No case named by: {}. The program exits with code 0.\n'.
              format(', '.join(list_of_wrong_folders)))
        exit(0)

    # -------------------------------------------------Test running process--------------------------------------------
    file_forecast_path = ''
    file_station_path = ''
    file_observation_path = ''
    extra_parameter = ''
    expected_value = 'SUCCESS'
    test_run = None
    arrow_line = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
    ripple_line = '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

    if verbosity:
        print('\n\n\n================================================================================')
        print('\n', '                            ', 'TESTING PROCESS: ')
        print('\n================================================================================')
    for folder in list_of_folders:
        current_case_path = './' + str(folder)
        for (dir_path, dir_names, file_names) in os.walk(current_case_path):  # Walk into each case.
            for file_name in file_names:
                extra_parameter = ''  # Set a default value for extra_parameter as not all cases need it.
                if file_name == 'forecast.xml':
                    file_forecast_path = os.path.join(test_suite_path + dir_path[1:], 'forecast.xml')
                if file_name == 'station.xml':
                    file_station_path = os.path.join(test_suite_path + dir_path[1:], 'station.xml')
                if file_name == 'observation.xml':
                    file_observation_path = os.path.join(test_suite_path + dir_path[1:], 'observation.xml')
                if file_name == 'config.json':
                    os.chdir(current_case_path)  # Change the current path to the current case directory.
                    with open('config.json') as f:
                        data = json.load(f)
                        for key, value in data.items():
                            if key == 'addition_to_command_line':
                                if value is not None:
                                    extra_parameter = value
                            if key == 'expected_running_result':
                                if value == 'FAILURE':
                                    expected_value = 'FAILURE'
                                elif value == 'SUCCESS':
                                    expected_value = 'SUCCESS'
                                else:
                                    print('Please verify the format/syntax of the config.json file.')
                            if key == 'error_tolerance':
                                error_value = value

            file_output_path = os.path.join(test_suite_path + dir_path[1:], 'roadcast_test_suite_run.xml')
            os.chdir(test_suite_path)  # Change back the path so as to make the function call.
            command_to_run = 'python3 ../../../../../src/frontend/metro.py {} --input-forecast {} --input-station {} ' \
                             '--input-observation {} --output-roadcast {}'.format(extra_parameter, file_forecast_path,
                                                                                  file_station_path,
                                                                                  file_observation_path,
                                                                                  file_output_path)
            if verbosity:
                try:
                    print('\n>>>>>>>>>>>>>>>>>>>>>>>> {} starts to run...... <<<<<<<<<<<<<<<<<<<<<<<<<<'
                          '<'.format(folder))
                    if expected_value == 'FAILURE':
                        print('>>>>>>>>>>>>>>>>>>>>>>>> {} is expected to FAIL...... <<<<<<<<<<<<<<<<<<'
                              '<<<'.format(folder))
                    print('\n\n')
                    test_run = subprocess.run(command_to_run, shell=True, check=True)
                    print('\n\n{}'.format(arrow_line))
                    print('\nError Tolerance:                  {}\n'.format(error_value))
                    print("roadcast_reference.xml            roadcast_test_suite_run.xml"
                          "\n----------------------            ---------------------------")
                    process_xml_file(current_case_path, folder, error_value, verbosity=True)
                    process_test_result(folder, test_run.returncode, expected_value)
                except subprocess.CalledProcessError:
                    print('\n\n{}'.format(arrow_line))
                    print('No generated XML file to do the comparison.\n')
                    process_test_result(folder, 1, expected_value)
                    print('\n{}\n\n'.format(arrow_line))
                    continue
                os.chdir(test_suite_path)
                print('\n{}\n\n'.format(arrow_line))

            elif not verbosity:
                test_run = subprocess.run(command_to_run, shell=True, stdout=subprocess.DEVNULL,
                                          stderr=subprocess.STDOUT)
                process_xml_file(current_case_path, folder, error_value, verbosity=False)
                process_test_result(folder, test_run.returncode, expected_value)
                os.chdir(test_suite_path)
            # ---------------------------------------Clean up the output XML file---------------------------------------
            if args.clean:
                try:
                    os.remove(current_case_path + '/roadcast_test_suite_run.xml')
                except FileNotFoundError as e:
                    continue
    # ----------------------------------------------Summary after test running------------------------------------------
    print('\n\n\n================================================================================')
    print('\n', '                            ', 'AFTER RUNNING SUMMARY: ')
    print('\n================================================================================')
    print('Total number of test cases ran:\t\t\t\t', str(num_of_folders), '.\nNumber of cases ran with SUCCESS:\t\t\t',
          str(num_of_success), '.\nNumber of cases  ran with FAILURE: \t\t\t', str(num_of_failure), '.')

    if len(XmlTree.list_of_diff_case) != 0:
        print('Cases having different XML file comparison result:\t', end=' ')
        print(*XmlTree.list_of_diff_case, sep=', ')
    print('\n\n\n')

    if (not verbosity) and (len(list_of_failure_cases) != 0):
        print('\n\n\n================================================================================')
        print('\n', '                        ', 'LIST OF FAILED RUNNING CASES: ')
        print('\n================================================================================')
        for case in list_of_failure_cases:
            print(case)
        print('\n\n\n')

    if (len(list_of_failure_cases) != 0) and verbosity:
        print('\n\n\n================================================================================')
        print('\n', '                        ', 'LIST OF FAILED RUNNING CASES: ')
        print('\n================================================================================')
        for case in list_of_failure_cases:
            print(case)
            print(ripple_line)
            print('Syntax to run this individual case: ')
            print('-----------------------------------')
            print('python3 ../../../../../src/frontend/metro.py {0} --input-forecast ../test_suite'
                  '/{1}/forecast.xml --input-station ../test_suite/{1}/station.xml --input'
                  '-observation ../test_suite/{1}/observation.xml --output-roadcast '
                  '../test_suite/{1}/roadcast_individual_run.xml'.format(extra_parameter, case))


if __name__ == '__main__':
    main()
